// @reliability: experimental
// @ai: assisted
//! AST-to-Solidity printer for the WARP verifier's native-EVM deployment path.
//!
//! Unlike the boolean-circuit `volar-evm-backend` (256-lane SIMD over a
//! bit-level Boolar circuit), this prints the *compiler IR* (`IrModule`)
//! directly as Solidity, keeping operations at the word level so the costs
//! stay low (see `volar-ir/docs/evm-warp-verifier-gas.md`):
//!
//! - `Galois128` maps to `uint256` and its `a * b` / `a + b` map to the
//!   measured `Gf128.mul` (~23K gas) / XOR (~68 gas), not a 79K-gate boolean
//!   expansion.
//! - Hashing is meant to be re-anchored to the native `KECCAK256` opcode.
//! - Memory is linear (Solidity `memory` arrays + `--via-ir` stack
//!   allocation), avoiding the raw backend's quadratic memory expansion.
//!
//! This is a deliberately small, growing subset — enough for the verifier's
//! field-arithmetic core (eq_evals / mle_eval / bundled-eval kernels). It is
//! not a general spec-to-Solidity compiler.

use core::fmt::Write as _;

use crate::ir::*;
use std::collections::BTreeMap;
use std::string::String;
use std::vec::Vec;
use std::format;

/// Errors from the Solidity printer (a construct outside the supported subset).
#[derive(Debug)]
pub struct SolError(pub String);

impl core::fmt::Display for SolError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        write!(f, "printer_sol: {}", self.0)
    }
}
impl std::error::Error for SolError {}

fn err<T>(m: impl Into<String>) -> core::result::Result<T, SolError> {
    Err(SolError(m.into()))
}

/// The measured `Gf128` library, inlined into every emitted contract so the
/// output is self-contained (mirrors `volar-ir/docs/native-evm/Gf128.sol`).
const GF128_LIB: &str = r#"library Gf128 {
    uint256 internal constant REDUCE = 0x87;
    uint256 internal constant HIGH = 1 << 127;
    uint256 internal constant MASK128 = (1 << 128) - 1;
    function mul(uint256 a, uint256 b) internal pure returns (uint256) {
        a &= MASK128;
        b &= MASK128;
        uint256 p = 0;
        for (uint256 i = 0; i < 128; i++) {
            if (b & 1 != 0) p ^= a;
            uint256 high = a & HIGH;
            a = (a << 1) & MASK128;
            if (high != 0) a ^= REDUCE;
            b >>= 1;
        }
        return p;
    }
}
"#;

struct Printer<'m> {
    /// var name -> IR type (for deciding field vs integer arithmetic).
    env: BTreeMap<String, IrType>,
    module: &'m IrModule<IrFunction>,
    /// Indentation level.
    indent: usize,
}

impl<'m> Printer<'m> {
    fn pad(&self) -> String {
        "    ".repeat(self.indent)
    }

    /// Is this type the GF(2^128) field element?
    fn is_g128(ty: &IrType) -> bool {
        matches!(ty, IrType::Primitive(PrimitiveType::Galois128))
    }

    /// Is this a machine integer we render as `uint256` with native ops?
    fn is_int(ty: &IrType) -> bool {
        matches!(
            ty,
            IrType::Primitive(
                PrimitiveType::U8
                    | PrimitiveType::U32
                    | PrimitiveType::U64
                    | PrimitiveType::Usize
                    | PrimitiveType::U128
                    | PrimitiveType::I64
                    | PrimitiveType::I128
            )
        )
    }

    /// Solidity type spelling for a value type.
    fn sol_ty(&self, ty: &IrType) -> core::result::Result<String, SolError> {
        match ty {
            IrType::Primitive(PrimitiveType::Galois128) => Ok("uint256".into()),
            IrType::Primitive(PrimitiveType::Bool) => Ok("bool".into()),
            IrType::Primitive(p) if Self::is_int(&IrType::Primitive(*p)) => Ok("uint256".into()),
            IrType::Array { elem, .. } => {
                let e = self.sol_ty(elem)?;
                Ok(format!("{e}[]"))
            }
            other => err(format!("unsupported type {other:?}")),
        }
    }

    fn array_len(&self, len: &ArrayLength) -> Option<usize> {
        match len {
            ArrayLength::Const(n) => Some(*n),
            ArrayLength::TypeNum(t) => Some(t.to_usize()),
            // Typenum constants the dialect writes as `U<n>` type params.
            ArrayLength::TypeParam(name) => name.strip_prefix('U').and_then(|s| s.parse().ok()),
            _ => None,
        }
    }

    /// Best-effort static type of an expression (enough to pick field vs int ops).
    fn infer_ty(&self, e: &IrExpr) -> Option<IrType> {
        match &e.kind {
            IrExprKind::Var(name) => self.env.get(name).cloned(),
            IrExprKind::Index { base, .. } => match self.infer_ty(base)? {
                IrType::Array { elem, .. } => Some(*elem),
                _ => None,
            },
            IrExprKind::Binary { op, left, right } => {
                use SpecBinOp::*;
                match op {
                    Eq | Ne | Lt | Le | Gt | Gt | And | Or => {
                        Some(IrType::Primitive(PrimitiveType::Bool))
                    }
                    _ => self.infer_ty(left).or_else(|| self.infer_ty(right)),
                }
            }
            IrExprKind::Cast { ty, .. } => Some((**ty).clone()),
            IrExprKind::Field { base, field } => {
                // Only need Galois128's `.0` (the u128 payload) for now.
                let _ = (base, field);
                None
            }
            IrExprKind::Call { func, .. } => {
                // The `Galois128(..)` tuple-struct constructor yields a field element.
                match &func.kind {
                    IrExprKind::Var(n) if n == "Galois128" => {
                        Some(IrType::Primitive(PrimitiveType::Galois128))
                    }
                    _ => None,
                }
            }
            IrExprKind::MethodCall { .. } => None,
            // `Array::<T, N>::default()` / `T::default()` carries its type.
            IrExprKind::DefaultValue { ty } => ty.as_deref().cloned(),
            _ => None,
        }
    }

    fn render_lit(&self, lit: &IrLit) -> core::result::Result<String, SolError> {
        match lit {
            IrLit::Int(i) => Ok(format!("{i}")),
            IrLit::Byte(b) => Ok(format!("{b}")),
            IrLit::Bool(b) => Ok(format!("{b}")),
            IrLit::Unit => Ok("0".into()),
            other => err(format!("unsupported literal {other:?}")),
        }
    }

    fn render_expr(&mut self, e: &IrExpr) -> core::result::Result<String, SolError> {
        match &e.kind {
            IrExprKind::Lit(l) => self.render_lit(l),
            IrExprKind::Var(n) => Ok(n.clone()),
            IrExprKind::Index { base, index } => {
                let b = self.render_expr(base)?;
                let i = self.render_expr(index)?;
                Ok(format!("{b}[{i}]"))
            }
            IrExprKind::Field { base, field } => {
                let b = self.render_expr(base)?;
                // Galois128(x) field access `.0` unwraps to the bare uint256.
                if field == "0" {
                    Ok(b)
                } else {
                    Ok(format!("{b}.{field}"))
                }
            }
            IrExprKind::Cast { expr, .. } => self.render_expr(expr),
            IrExprKind::Unary { op, expr } => {
                let x = self.render_expr(expr)?;
                match op {
                    SpecUnaryOp::Neg => Ok(format!("(-{x})")),
                    SpecUnaryOp::Not => Ok(format!("(!{x})")),
                    SpecUnaryOp::Deref => Ok(x),
                    SpecUnaryOp::Ref | SpecUnaryOp::RefMut => Ok(x),
                }
            }
            IrExprKind::Binary { op, left, right } => self.render_binary(*op, left, right),
            IrExprKind::Block(b) => {
                // Expression block: render as an immediately-scoped sequence is
                // not expressible inline in Solidity; require it be a stmt.
                let _ = b;
                err("inline block expression not supported; hoist to statements")
            }
            IrExprKind::If { cond, then_branch, else_branch } => {
                // Ternary only when both branches are a single expr.
                let c = self.render_expr(cond)?;
                if then_branch.stmts.is_empty() {
                    if let (Some(t), Some(el)) = (&then_branch.expr, else_branch) {
                        let t = self.render_expr(t)?;
                        let el = self.render_expr(el)?;
                        return Ok(format!("({c} ? {t} : {el})"));
                    }
                }
                err("statement-level if must be rendered as a statement")
            }
            IrExprKind::Call { func, args } => self.render_call(func, args),
            IrExprKind::MethodCall { receiver, method, args, .. } => {
                self.render_method_call(receiver, method, args)
            }
            IrExprKind::Return(e) => {
                let _ = e;
                err("return must be rendered as a statement")
            }
            IrExprKind::DefaultValue { ty } => {
                // Array::default() -> a fresh zero-filled memory array.
                match ty.as_deref() {
                    Some(IrType::Array { elem, len, .. }) => {
                        let e = self.sol_ty(elem)?;
                        let n = self.array_len(len).ok_or_else(|| SolError("default array needs a concrete length".into()))?;
                        Ok(format!("new {e}[]({n})"))
                    }
                    Some(IrType::Primitive(PrimitiveType::Galois128)) => Ok("0".into()),
                    Some(t) if Self::is_int(t) => Ok("0".into()),
                    _ => err(format!("unsupported default value {ty:?}")),
                }
            }
            IrExprKind::TypenumUsize { ty } => {
                match ty.as_ref() {
                    IrType::Primitive(PrimitiveType::Usize) => Ok("0".into()),
                    IrType::TypeParam(n) => n.strip_prefix('U').and_then(|s| s.parse::<usize>().ok()).map(|u| format!("{u}")).ok_or_else(|| SolError(format!("typenum {n}"))),
                    _ => err(format!("typenum usize: {ty:?}")),
                }
            }
            other => err(format!("unsupported expr {:?}", core::mem::discriminant(other))),
        }
    }

    fn render_binary(
        &mut self,
        op: SpecBinOp,
        left: &IrExpr,
        right: &IrExpr,
    ) -> core::result::Result<String, SolError> {
        let lt = self.infer_ty(left);
        let is_field = lt.as_ref().map(Self::is_g128).unwrap_or(false);
        let l = self.render_expr(left)?;
        let r = self.render_expr(right)?;
        use SpecBinOp::*;
        Ok(match op {
            Add if is_field => format!("({l} ^ {r})"),
            Sub if is_field => format!("({l} ^ {r})"),
            Mul if is_field => format!("Gf128.mul({l}, {r})"),
            Add => format!("({l} + {r})"),
            Sub => format!("({l} - {r})"),
            Mul => format!("({l} * {r})"),
            Div => format!("({l} / {r})"),
            Rem => format!("({l} % {r})"),
            BitAnd => format!("({l} & {r})"),
            BitOr => format!("({l} | {r})"),
            BitXor => format!("({l} ^ {r})"),
            Shl => format!("({l} << {r})"),
            Shr => format!("({l} >> {r})"),
            Eq => format!("({l} == {r})"),
            Ne => format!("({l} != {r})"),
            Lt => format!("({l} < {r})"),
            Le => format!("({l} <= {r})"),
            Gt => format!("({l} > {r})"),
            Ge => format!("({l} >= {r})"),
            And => format!("({l} && {r})"),
            Or => format!("({l} || {r})"),
        })
    }

    fn render_call(&mut self, func: &IrExpr, args: &[IrExpr]) -> core::result::Result<String, SolError> {
        // Galois128(x) constructor → the bare payload.
        if let IrExprKind::Var(name) = &func.kind {
            if name == "Galois128" && args.len() == 1 {
                return self.render_expr(&args[0]);
            }
        }
        let fname = match &func.kind {
            IrExprKind::Var(n) => n.clone(),
            IrExprKind::Path { segments, .. } => segments.join("_"),
            _ => return err("unsupported call target"),
        };
        let mut rendered = Vec::new();
        for a in args {
            rendered.push(self.render_expr(a)?);
        }
        // Native Keccak extern: a spec-level `keccak_pair(l, r)` (the WARP
        // Merkle node hash) maps to the native KECCAK256 opcode over the two
        // 32-byte words, truncated to the 128-bit node width. This is the
        // re-anchoring from the spec's bit-level SHAKE128 sponge to the
        // EVM-native hash that makes the verifier's hashing cheap.
        if fname == "keccak_pair" && rendered.len() == 2 {
            return Ok(format!(
                "((uint256(keccak256(abi.encodePacked(bytes32({}), bytes32({}))))) & ((1 << 128) - 1))",
                rendered[0], rendered[1]
            ));
        }
        Ok(format!("{}({})", fname, rendered.join(", ")))
    }

    fn render_method_call(
        &mut self,
        receiver: &IrExpr,
        method: &MethodKind,
        args: &[IrExpr],
    ) -> core::result::Result<String, SolError> {
        let recv = self.render_expr(receiver)?;
        let mname = match method {
            MethodKind::Other(n) => n.clone(),
            MethodKind::Known(s) => format!("{s:?}"),
            MethodKind::Vole(v) => format!("{v:?}"),
        };
        let mut rendered = Vec::new();
        for a in args {
            rendered.push(self.render_expr(a)?);
        }
        Ok(format!("{recv}.{mname}({})", rendered.join(", ")))
    }

    fn render_stmt(&mut self, stmt: &IrStmt, out: &mut String) -> core::result::Result<(), SolError> {
        match &stmt.kind {
            IrStmtKind::Let { pattern, ty, init } => {
                let name = pattern_name(pattern);
                // The declared type, or the inferred type of the initializer.
                let init_ty: Option<IrType> = ty.clone().or_else(|| {
                    init.as_ref().and_then(|e| self.infer_ty(e))
                });
                let init_ty = init_ty.map(|t| self.resolve_alias(t));
                if let Some(t) = &init_ty {
                    self.env.insert(name.clone(), t.clone());
                }
                let ty_s = match &init_ty {
                    Some(t) => self.sol_ty(t)?,
                    None => "uint256".into(),
                };
                let decl = if matches!(init_ty, Some(IrType::Array { .. })) {
                    format!("{ty_s} memory {name}")
                } else {
                    format!("{ty_s} {name}")
                };
                match init {
                    Some(e) => {
                        let v = self.render_expr(e)?;
                        writeln!(out, "{}{} = {};", self.pad(), decl, v).ok();
                    }
                    None => {
                        writeln!(out, "{}{};", self.pad(), decl).ok();
                    }
                }
                Ok(())
            }
            IrStmtKind::Semi(e) | IrStmtKind::Expr(e) => self.render_stmt_expr(e, out),
            IrStmtKind::LetElse { .. } => err("let-else not supported"),
        }
    }

    /// Resolve a module type alias / nominal to its structural type when we can.
    fn resolve_alias(&self, ty: IrType) -> IrType {
        ty
    }

    /// Render an expression in statement position (assignments, loops, ifs, calls).
    fn render_stmt_expr(&mut self, e: &IrExpr, out: &mut String) -> core::result::Result<(), SolError> {
        match &e.kind {
            IrExprKind::BoundedLoop { var, start, end, body, .. } => {
                let s = self.render_expr(start)?;
                let en = self.render_expr(end)?;
                writeln!(out, "{}for (uint256 {var} = {s}; {var} < {en}; {var}++) {{", self.pad()).ok();
                self.indent += 1;
                self.env.insert(var.clone(), IrType::Primitive(PrimitiveType::Usize));
                self.render_block(body, out)?;
                self.indent -= 1;
                writeln!(out, "{}}}", self.pad()).ok();
                Ok(())
            }
            IrExprKind::If { cond, then_branch, else_branch } => {
                let c = self.render_expr(cond)?;
                writeln!(out, "{}if ({c}) {{", self.pad()).ok();
                self.indent += 1;
                self.render_block(then_branch, out)?;
                self.indent -= 1;
                match else_branch {
                    Some(el) => {
                        writeln!(out, "{}}} else {{", self.pad()).ok();
                        self.indent += 1;
                        match &el.kind {
                            IrExprKind::Block(b) => self.render_block(b, out)?,
                            _ => self.render_stmt_expr(el, out)?,
                        }
                        self.indent -= 1;
                        writeln!(out, "{}}}", self.pad()).ok();
                    }
                    None => {
                        writeln!(out, "{}}}", self.pad()).ok();
                    }
                }
                Ok(())
            }
            IrExprKind::Assign { left, right } => {
                let t = self.render_expr(left)?;
                let v = self.render_expr(right)?;
                writeln!(out, "{}{t} = {v};", self.pad()).ok();
                Ok(())
            }
            IrExprKind::AssignOp { op, left, right } => {
                let t = self.render_expr(left)?;
                let is_field = self.infer_ty(left).map(|t| Self::is_g128(&t)).unwrap_or(false);
                let v = self.render_expr(right)?;
                use SpecBinOp::*;
                let line = match op {
                    Add if is_field => format!("{t} = {t} ^ {v};"),
                    Sub if is_field => format!("{t} = {t} ^ {v};"),
                    Mul if is_field => format!("{t} = Gf128.mul({t}, {v});"),
                    Add => format!("{t} += {v};"),
                    Sub => format!("{t} -= {v};"),
                    Mul => format!("{t} *= {v};"),
                    BitXor => format!("{t} ^= {v};"),
                    BitAnd => format!("{t} &= {v};"),
                    BitOr => format!("{t} |= {v};"),
                    other => return err(format!("unsupported assignop {other:?}")),
                };
                writeln!(out, "{}{line}", self.pad()).ok();
                Ok(())
            }
            IrExprKind::Return(e) => {
                match e {
                    Some(x) => {
                        let v = self.render_expr(x)?;
                        writeln!(out, "{}return {v};", self.pad()).ok();
                    }
                    None => {
                        writeln!(out, "{}return;", self.pad()).ok();
                    }
                }
                Ok(())
            }
            _ => {
                let v = self.render_expr(e)?;
                writeln!(out, "{}{v};", self.pad()).ok();
                Ok(())
            }
        }
    }

    fn render_block(&mut self, b: &IrBlock, out: &mut String) -> core::result::Result<(), SolError> {
        for s in &b.stmts {
            self.render_stmt(s, out)?;
        }
        if let Some(e) = &b.expr {
            // Trailing expr = the value; treat as a return when it's the body tail.
            self.render_stmt_expr_tail(e, out)?;
        }
        Ok(())
    }

    fn render_stmt_expr_tail(&mut self, e: &IrExpr, out: &mut String) -> core::result::Result<(), SolError> {
        match &e.kind {
            IrExprKind::Return(_) => self.render_stmt_expr(e, out),
            _ => {
                let v = self.render_expr(e)?;
                writeln!(out, "{}return {v};", self.pad()).ok();
                Ok(())
            }
        }
    }

    fn render_function(&mut self, f: &IrFunction, out: &mut String) -> core::result::Result<(), SolError> {
        self.env.clear();
        let mut params = Vec::new();
        for p in &f.params {
            let ty = self.sol_ty(&p.ty)?;
            self.env.insert(p.name.clone(), p.ty.clone());
            let loc = if matches!(p.ty, IrType::Array { .. }) { " memory" } else { "" };
            params.push(format!("{ty}{loc} {}", p.name));
        }
        let ret = match &f.return_type {
            Some(t) => format!(" returns ({})", self.sol_ty_ret(t)?),
            None => String::new(),
        };
        writeln!(
            out,
            "{}function {}({}) public pure{} {{",
            self.pad(),
            f.name,
            params.join(", "),
            ret
        )
        .ok();
        self.indent += 1;
        self.render_block(&f.body, out)?;
        self.indent -= 1;
        writeln!(out, "{}}}", self.pad()).ok();
        Ok(())
    }

    fn sol_ty_ret(&self, ty: &IrType) -> core::result::Result<String, SolError> {
        match ty {
            IrType::Array { .. } => Ok(format!("{} memory", self.sol_ty(ty)?)),
            _ => self.sol_ty(ty),
        }
    }
}

fn pattern_name(p: &IrPattern) -> String {
    match p {
        IrPattern::Ident { name, .. } => name.clone(),
        _ => "_".into(),
    }
}

/// Print the named functions of `module` as a self-contained Solidity contract
/// (with the `Gf128` library inlined).
pub fn print_module_solidity(
    module: &IrModule<IrFunction>,
    fn_names: &[&str],
    contract: &str,
) -> core::result::Result<String, SolError> {
    let mut out = String::new();
    out.push_str("// SPDX-License-Identifier: MIT\npragma solidity ^0.8.24;\n\n");
    out.push_str(GF128_LIB);
    out.push('\n');
    writeln!(out, "contract {contract} {{").ok();
    let mut p = Printer {
        env: BTreeMap::new(),
        module,
        indent: 1,
    };
    for name in fn_names {
        let f = module
            .functions
            .iter()
            .find(|f| f.name == *name)
            .ok_or_else(|| SolError(format!("function {name} not found")))?;
        p.render_function(f, &mut out)?;
    }
    out.push_str("}\n");
    Ok(out)
}
