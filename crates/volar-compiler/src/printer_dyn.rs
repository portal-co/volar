use core::fmt::Write;
#[cfg(feature = "std")]
use std::{collections::{BTreeMap, BTreeSet}, string::{String, ToString}, vec::Vec, format};

#[cfg(not(feature = "std"))]
use alloc::{collections::{BTreeMap, BTreeSet}, string::{String, ToString}, vec::Vec, format};

use crate::ir::*;

pub fn print_module_rust_dyn(module: &IrModule) -> String {
    let mut struct_witnesses = BTreeMap::new();
    for s in &module.structs {
        let mut witnesses = Vec::new();
        for p in &s.generics {
            if is_length_param(p, &s.fields) {
                witnesses.push(p.name.clone());
            }
        }
        struct_witnesses.insert(s.kind.to_string(), witnesses);
    }

    let mut out = String::new();
    writeln!(out, "#![allow(unused_variables, dead_code, unused_mut, unused_imports)]").unwrap();
    writeln!(out, "use alloc::vec::Vec;").unwrap();
    writeln!(out, "use alloc::vec;").unwrap();
    writeln!(out, "use core::ops::{{Add, Sub, Mul, Div, Rem, BitAnd, BitOr, BitXor, Neg, Not}};").unwrap();
    writeln!(out).unwrap();

    // Mock Bit and other types if not present
    writeln!(out, "#[derive(Clone, Copy, Default, Debug, PartialEq)] pub struct Bit(pub bool);").unwrap();
    writeln!(out, "impl core::ops::BitXor for Bit {{ type Output = Self; fn bitxor(self, rhs: Self) -> Self {{ Bit(self.0 ^ rhs.0) }} }}").unwrap();
    writeln!(out, "pub type Galois = u8;").unwrap();
    writeln!(out, "pub type Galois64 = u64;").unwrap();
    writeln!(out, "#[derive(Clone, Copy, Default, Debug, PartialEq)] pub struct BitsInBytes(pub u8);").unwrap();
    writeln!(out, "#[derive(Clone, Copy, Default, Debug, PartialEq)] pub struct BitsInBytes64(pub u64);").unwrap();
    writeln!(out, "impl core::ops::BitXor for BitsInBytes {{ type Output = Self; fn bitxor(self, rhs: Self) -> Self {{ BitsInBytes(self.0 ^ rhs.0) }} }}").unwrap();
    writeln!(out, "impl core::ops::BitXor for BitsInBytes64 {{ type Output = Self; fn bitxor(self, rhs: Self) -> Self {{ BitsInBytes64(self.0 ^ rhs.0) }} }}").unwrap();
    writeln!(out, "impl BitsInBytes {{ pub fn shl(self, n: u32) -> u8 {{ self.0 << n }} pub fn shr(self, n: u32) -> u8 {{ self.0 >> n }} }}").unwrap();
    writeln!(out, "impl BitsInBytes64 {{ pub fn shl(self, n: u32) -> u64 {{ self.0 << n }} pub fn shr(self, n: u32) -> u64 {{ self.0 >> n }} }}").unwrap();
    writeln!(out, "impl core::ops::Shl<u32> for BitsInBytes {{ type Output = Self; fn shl(self, rhs: u32) -> Self {{ BitsInBytes(self.0 << rhs) }} }}").unwrap();
    writeln!(out, "impl core::ops::Shr<u32> for BitsInBytes {{ type Output = Self; fn shr(self, rhs: u32) -> Self {{ BitsInBytes(self.0 >> rhs) }} }}").unwrap();
    writeln!(out, "impl core::ops::Shl<u32> for BitsInBytes64 {{ type Output = Self; fn shl(self, rhs: u32) -> Self {{ BitsInBytes64(self.0 << rhs) }} }}").unwrap();
    writeln!(out, "impl core::ops::Shr<u32> for BitsInBytes64 {{ type Output = Self; fn shr(self, rhs: u32) -> Self {{ BitsInBytes64(self.0 >> rhs) }} }}").unwrap();
    writeln!(out, "impl core::ops::Shl<u32> for Bit {{ type Output = Self; fn shl(self, rhs: u32) -> Self {{ self }} }}").unwrap();
    writeln!(out, "impl core::ops::Shr<u32> for Bit {{ type Output = Self; fn shr(self, rhs: u32) -> Self {{ self }} }}").unwrap();
    writeln!(out, "impl core::ops::BitAnd<usize> for BitsInBytes {{ type Output = usize; fn bitand(self, rhs: usize) -> usize {{ (self.0 as usize) & rhs }} }}").unwrap();
    writeln!(out, "impl core::ops::BitAnd<usize> for BitsInBytes64 {{ type Output = usize; fn bitand(self, rhs: usize) -> usize {{ (self.0 as usize) & rhs }} }}").unwrap();
    writeln!(out, "impl core::ops::Shr<usize> for BitsInBytes {{ type Output = usize; fn shr(self, rhs: usize) -> usize {{ (self.0 as usize) >> rhs }} }}").unwrap();
    writeln!(out, "impl core::ops::Shr<usize> for BitsInBytes64 {{ type Output = usize; fn shr(self, rhs: usize) -> usize {{ (self.0 as usize) >> rhs }} }}").unwrap();
    writeln!(out, "pub struct GenericArray;").unwrap();
    writeln!(out, "impl GenericArray {{ pub fn default<T: Default>() -> Vec<T> {{ Vec::new() }} pub fn generate<T, F: FnMut(usize) -> T>(n: usize, mut f: F) -> Vec<T> {{ (0..n).map(f).collect() }} }}").unwrap();
    writeln!(out, "pub struct CommitmentCore;").unwrap();
    writeln!(out, "impl CommitmentCore {{ pub fn commit<T>(_: T, _: &u64) -> Vec<u8> {{ Vec::new() }} }}").unwrap();
    writeln!(out, "pub fn ilog2(x: usize) -> u32 {{ (usize::BITS - x.leading_zeros() - 1) }}").unwrap();
    writeln!(out, "pub trait New {{ fn new() -> Self; }}").unwrap();
    writeln!(out, "impl New for u8 {{ fn new() -> Self {{ 0 }} }}").unwrap();
    writeln!(out, "impl New for u64 {{ fn new() -> Self {{ 0 }} }}").unwrap();
    writeln!(out, "impl New for Bit {{ fn new() -> Self {{ Bit(false) }} }}").unwrap();
    writeln!(out, "impl New for BitsInBytes {{ fn new() -> Self {{ BitsInBytes(0) }} }}").unwrap();
    writeln!(out, "impl New for BitsInBytes64 {{ fn new() -> Self {{ BitsInBytes64(0) }} }}").unwrap();
    writeln!(out, "pub trait DefaultNew: Default {{ fn new() -> Self {{ Self::default() }} }}").unwrap();
    writeln!(out, "impl<T: Default> DefaultNew for T {{}}").unwrap();
    writeln!(out, "pub trait ToUsize {{ fn to_usize(&self) -> usize; }}").unwrap();
    writeln!(out, "impl ToUsize for usize {{ fn to_usize(&self) -> usize {{ *self }} }}").unwrap();
    writeln!(out, "pub struct BlockSizeDyn;").unwrap();
    writeln!(out, "pub struct OutputSizeDyn;").unwrap();
    writeln!(out, "impl BlockSizeDyn {{ pub fn to_usize(&self) -> usize {{ 16 }} }}").unwrap();
    writeln!(out, "impl OutputSizeDyn {{ pub fn to_usize(&self) -> usize {{ 16 }} }}").unwrap();
    writeln!(out, "impl Default for BlockSizeDyn {{ fn default() -> Self {{ BlockSizeDyn }} }}").unwrap();
    writeln!(out, "impl Default for OutputSizeDyn {{ fn default() -> Self {{ OutputSizeDyn }} }}").unwrap();
    writeln!(out, "pub struct B; impl B {{ pub fn from<T>(_: T) -> usize {{ 0 }} pub const BlockSize: BlockSizeDyn = BlockSizeDyn; }}").unwrap();
    writeln!(out, "pub struct D; impl D {{ pub fn new() -> Self {{ D }} pub fn to_usize(&self) -> usize {{ 0 }} pub fn finalize(&self) -> Vec<u8> {{ Vec::new() }} pub fn update(&mut self, _: &[u8]) {{}} pub const OutputSize: OutputSizeDyn = OutputSizeDyn; }}").unwrap();
    writeln!(out, "pub trait DefaultVal {{ fn default_val() -> Self; }}").unwrap();
    writeln!(out, "impl DefaultVal for u8 {{ fn default_val() -> Self {{ 0 }} }}").unwrap();
    writeln!(out, "impl DefaultVal for u64 {{ fn default_val() -> Self {{ 0 }} }}").unwrap();
    writeln!(out, "impl DefaultVal for Bit {{ fn default_val() -> Self {{ Bit(false) }} }}").unwrap();
    writeln!(out, "impl DefaultVal for BitsInBytes {{ fn default_val() -> Self {{ BitsInBytes(0) }} }}").unwrap();
    writeln!(out, "impl DefaultVal for BitsInBytes64 {{ fn default_val() -> Self {{ BitsInBytes64(0) }} }}").unwrap();
    writeln!(out, "impl<T: DefaultVal> DefaultVal for Vec<T> {{ fn default_val() -> Self {{ Vec::new() }} }}").unwrap();
    writeln!(out, "impl<T> core::ops::Add<T> for PolyDyn<T> where T: core::ops::Add<Output=T> + Clone {{ type Output = Self; fn add(self, rhs: T) -> Self {{ todo!() }} }}").unwrap();
    writeln!(out, "impl<T> core::ops::Mul<T> for PolyDyn<T> where T: core::ops::Mul<Output=T> + Clone {{ type Output = Self; fn mul(self, rhs: T) -> Self {{ todo!() }} }}").unwrap();
    writeln!(out, "impl<T> core::ops::Add<Self> for VopeDyn<T> where T: core::ops::Add<Output=T> + Clone {{ type Output = Self; fn add(self, rhs: Self) -> Self {{ todo!() }} }}").unwrap();
    writeln!(out, "impl<T> core::ops::Sub<Self> for VopeDyn<T> where T: core::ops::Sub<Output=T> + Clone {{ type Output = Self; fn sub(self, rhs: Self) -> Self {{ todo!() }} }}").unwrap();
    writeln!(out, "impl<T> core::ops::Mul<T> for VopeDyn<T> where T: core::ops::Mul<Output=T> + Clone {{ type Output = Self; fn mul(self, rhs: T) -> Self {{ todo!() }} }}").unwrap();
    writeln!(out, "pub type OutputDyn = Vec<u8>;").unwrap();
    writeln!(out, "pub type Q = u8;").unwrap();
    writeln!(out, "pub type A = u8;").unwrap();
    writeln!(out, "pub type Delta = u8;").unwrap();

    writeln!(out, "pub struct PolyInputPoolDyn<'a, T> {{").unwrap();
    writeln!(out, "    pub t: usize,").unwrap();
    writeln!(out, "    pub n: usize,").unwrap();
    writeln!(out, "    pub x: usize,").unwrap();
    writeln!(out, "    pub inputs: &'a Vec<T>,").unwrap();
    writeln!(out, "    pub indices: Vec<Vec<usize>>,").unwrap();
    writeln!(out, "}}").unwrap();

    for s in &module.structs {
        if s.kind.to_string() == "PolyInputPool" { continue; }
        write_struct_dyn(&mut out, s);
        writeln!(out).unwrap();
    }

    for i in &module.impls {
        write_impl_dyn(&mut out, i, &struct_witnesses);
        writeln!(out).unwrap();
    }

    for f in &module.functions {
        write_function_dyn(&mut out, f, 0, &IrType::Unit, &struct_witnesses);
        writeln!(out).unwrap();
    }

    out
}

fn write_struct_dyn(out: &mut String, s: &IrStruct) {
    let name = format!("{}Dyn", s.kind);
    write!(out, "#[derive(Clone, Debug, Default)]\npub struct {}", name).unwrap();
    
    let mut type_params = Vec::new();
    let mut length_params = Vec::new();
    
    for p in &s.generics {
        if is_length_param(p, &s.fields) {
            length_params.push(p.name.clone());
        } else if p.kind == IrGenericParamKind::Type {
            type_params.push(p.name.clone());
        }
    }

    if !type_params.is_empty() {
        write!(out, "<{}>", type_params.join(", ")).unwrap();
    }
    
    if s.is_tuple {
        write!(out, "(").unwrap();
        for lp in &length_params {
            write!(out, "pub usize, ").unwrap();
        }
        for (i, f) in s.fields.iter().enumerate() {
            if i > 0 || !length_params.is_empty() { write!(out, ", ").unwrap(); }
            write_type_dyn(out, &f.ty);
        }
        writeln!(out, ");").unwrap();
    } else {
        writeln!(out, " {{").unwrap();
        for lp in &length_params {
            writeln!(out, "    pub {}: usize,", lp.to_lowercase()).unwrap();
        }
        
        for f in &s.fields {
            write!(out, "    pub {}: ", f.name).unwrap();
            write_type_dyn(out, &f.ty);
            writeln!(out, ",").unwrap();
        }
        writeln!(out, "}}").unwrap();
    }
}

fn is_length_param(p: &IrGenericParam, fields: &[IrField]) -> bool {
    for bound in &p.bounds {
        if matches!(bound.trait_kind, TraitKind::Crypto(CryptoTrait::ArrayLength)) {
            return true;
        }
    }
    for field in fields {
        if type_uses_as_len(&field.ty, &p.name) {
            return true;
        }
    }
    is_likely_len_param(&p.name)
}

fn is_likely_len_param(name: &str) -> bool {
    matches!(name, "N" | "M" | "K" | "L" | "B" | "D" | "X" | "U" | "S" | "T" | "K2" if name.len() <= 2) || 
    name.starts_with('N') && name.chars().nth(1).map_or(false, |c| c.is_ascii_digit())
}

fn type_uses_as_len(ty: &IrType, name: &str) -> bool {
    match ty {
        IrType::Array { len: ArrayLength::TypeParam(p), .. } if p == name => true,
        IrType::Array { elem, .. } => type_uses_as_len(elem, name),
        IrType::Struct { type_args, .. } => type_args.iter().any(|arg| type_uses_as_len(arg, name)),
        IrType::Tuple(elems) => elems.iter().any(|e| type_uses_as_len(e, name)),
        IrType::Reference { elem, .. } => type_uses_as_len(elem, name),
        _ => false,
    }
}

fn write_impl_dyn(out: &mut String, i: &IrImpl, struct_witnesses: &BTreeMap<String, Vec<String>>) {
    let self_name = match &i.self_ty {
        IrType::Struct { kind, .. } => format!("{}Dyn", kind),
        _ => return,
    };

    write!(out, "impl").unwrap();
    let mut type_params = Vec::new();
    for p in &i.generics {
         if p.kind == IrGenericParamKind::Type && !is_likely_len_param(&p.name) {
             type_params.push(p.name.clone());
         }
    }
    if !type_params.is_empty() {
        write!(out, "<{}>", type_params.join(", ")).unwrap();
    }

    write!(out, " {}", self_name).unwrap();
    if !type_params.is_empty() {
        write!(out, "<{}>", type_params.join(", ")).unwrap();
    }

    writeln!(out, " {{").unwrap();
    for item in &i.items {
        match item {
            IrImplItem::Method(f) => write_function_dyn(out, f, 1, &i.self_ty, struct_witnesses),
            _ => {}
        }
    }
    writeln!(out, "}}").unwrap();
}

fn write_function_dyn(out: &mut String, f: &IrFunction, level: usize, self_ty: &IrType, struct_witnesses: &BTreeMap<String, Vec<String>>) {
    let indent = "    ".repeat(level);
    write!(out, "{}pub fn {}(", indent, f.name).unwrap();
    
    let mut len_params_in_func = Vec::new();
    for p in &f.generics {
        if is_likely_len_param(&p.name) {
            len_params_in_func.push(p.name.clone());
        }
    }

    if let Some(r) = f.receiver {
        match r {
            IrReceiver::Value => write!(out, "self").unwrap(),
            IrReceiver::Ref => write!(out, "&self").unwrap(),
            IrReceiver::RefMut => write!(out, "&mut self").unwrap(),
        }
    }

    for (i, lp) in len_params_in_func.iter().enumerate() {
        if i > 0 || f.receiver.is_some() { write!(out, ", ").unwrap(); }
        write!(out, "{}_len: usize", lp.to_lowercase()).unwrap();
    }

    for (i, p) in f.params.iter().enumerate() {
        if i > 0 || f.receiver.is_some() || !len_params_in_func.is_empty() { write!(out, ", ").unwrap(); }
        write!(out, "{}: ", p.name).unwrap();
        write_type_dyn(out, &p.ty);
    }
    write!(out, ")").unwrap();
    if let Some(ret) = &f.return_type {
        write!(out, " -> ").unwrap();
        write_type_dyn(out, ret);
    }
    writeln!(out).unwrap();

    let indent_body = "    ".repeat(level + 1);
    writeln!(out, "{}{{", indent).unwrap();

    // Unpack witnesses from self
    if f.receiver.is_some() {
        if let IrType::Struct { kind, .. } = self_ty {
            if let Some(witnesses) = struct_witnesses.get(&kind.to_string()) {
                for w in witnesses {
                    writeln!(out, "{}let {} = self.{};", indent_body, w.to_lowercase(), w.to_lowercase()).unwrap();
                }
            }
        }
    }

    // Heuristic: for many cryptographic functions, 'n' is a standard parameter name for Vec length
    // If n is used in the body but not defined, we might need to derive it
    // But for now, let's just make sure n, t, k etc. are available if they are struct fields

    // Unpack witnesses from arguments
    for (i, lp) in len_params_in_func.iter().enumerate() {
        writeln!(out, "{}let {} = {}_len;", indent_body, lp.to_lowercase(), lp.to_lowercase()).unwrap();
    }

    for stmt in &f.body.stmts {
        write_stmt_dyn(out, stmt, level + 1, struct_witnesses);
    }
    if let Some(e) = &f.body.expr {
        write!(out, "{}    ", indent).unwrap();
        if let IrExpr::Var(v) = e.as_ref() {
            if v == "sum" {
                write!(out, "sum").unwrap();
            } else {
                write_expr_dyn(out, e, struct_witnesses);
            }
        } else {
            write_expr_dyn(out, e, struct_witnesses);
        }
        writeln!(out).unwrap();
    }
    writeln!(out, "{}}}", indent).unwrap();
}

fn write_type_dyn(out: &mut String, ty: &IrType) {
    match ty {
        IrType::Primitive(p) => write!(out, "{}", p).unwrap(),
        IrType::Array { elem, .. } | IrType::Vector { elem } => {
            write!(out, "Vec<").unwrap();
            write_type_dyn(out, elem);
            write!(out, ">").unwrap();
        }
        IrType::Struct { kind, type_args } => {
            if kind.to_string() == "PolyInputPool" {
                write!(out, "PolyInputPoolDyn<'_").unwrap();
            } else {
                write!(out, "{}Dyn", kind).unwrap();
            }
            let mut filtered_args = Vec::new();
            for arg in type_args {
                if let IrType::TypeParam(n) = arg {
                    if is_likely_len_param(n) { continue; }
                }
                filtered_args.push(arg);
            }
            if !filtered_args.is_empty() {
                if kind.to_string() == "PolyInputPool" {
                    write!(out, ", ").unwrap();
                } else {
                    write!(out, "<").unwrap();
                }
                for (i, arg) in filtered_args.iter().enumerate() {
                    if i > 0 { write!(out, ", ").unwrap(); }
                    write_type_dyn(out, arg);
                }
                write!(out, ">").unwrap();
            } else if kind.to_string() == "PolyInputPool" {
                write!(out, ">").unwrap();
            }
        }
        IrType::TypeParam(p) => write!(out, "{}", p).unwrap(),
        IrType::Tuple(elems) => {
            write!(out, "(").unwrap();
            for (i, elem) in elems.iter().enumerate() {
                if i > 0 { write!(out, ", ").unwrap(); }
                write_type_dyn(out, elem);
            }
            write!(out, ")").unwrap();
        }
        IrType::Unit => write!(out, "()").unwrap(),
        IrType::Reference { mutable, elem } => {
            write!(out, "&{}", if *mutable { "mut " } else { "" }).unwrap();
            write_type_dyn(out, elem);
        }
        _ => write!(out, "_").unwrap(),
    }
}

fn write_stmt_dyn(out: &mut String, stmt: &IrStmt, level: usize, struct_witnesses: &BTreeMap<String, Vec<String>>) {
    let indent = "    ".repeat(level);
    match stmt {
        IrStmt::Let { pattern, ty, init } => {
            let mut pat_str = String::new();
            write_pattern_dyn(&mut pat_str, pattern);
            
            if pat_str == "_" {
                write!(out, "{}let ", indent).unwrap();
            } else {
                write!(out, "{}let mut ", indent).unwrap();
            }
            write!(out, "{}", pat_str).unwrap();
            
            if let Some(t) = ty {
                write!(out, ": ").unwrap();
                write_type_dyn(out, t);
            }
            if let Some(i) = init {
                write!(out, " = ").unwrap();
                write_expr_dyn(out, i, struct_witnesses);
            }
            writeln!(out, ";").unwrap();
        }
        IrStmt::Semi(e) => {
            write!(out, "{}", indent).unwrap();
            write_expr_dyn(out, e, struct_witnesses);
            writeln!(out, ";").unwrap();
        }
        IrStmt::Expr(e) => {
            write!(out, "{}", indent).unwrap();
            write_expr_dyn(out, e, struct_witnesses);
            writeln!(out).unwrap();
        }
    }
}

fn write_expr_dyn(out: &mut String, expr: &IrExpr, struct_witnesses: &BTreeMap<String, Vec<String>>) {
    match expr {
        IrExpr::Lit(l) => write!(out, "{}", l).unwrap(),
        IrExpr::Var(v) => {
            if is_likely_len_param(v) {
                write!(out, "{}", v.to_lowercase()).unwrap();
            } else if matches!(v.as_str(), "delta" | "q" | "u" | "v" | "c0" | "c1" | "indices" | "inputs" | "commit" | "per_byte" | "bad" | "openings") {
                write!(out, "self.{}", v).unwrap();
            } else if matches!(v.as_str(), "BlockSize" | "OutputSize") {
                write!(out, "16").unwrap(); // Default mock constants
            } else if v == "sum" || v == "i" || v == "j" || v == "k" || v == "m" || v == "l" || v == "n" || v == "o" || v == "a" || v == "next" || v == "prev" || v == "u1" || v == "u2" || v == "v1" || v == "v2" || v == "q1" || v == "q2" || v == "d1" || v == "d2" {
                write!(out, "{}", v).unwrap();
            } else {
                write!(out, "{}", v).unwrap();
            }
        }
        IrExpr::Binary { op, left, right } => {
            let mut left_str = String::new();
            write_expr_dyn(&mut left_str, left, struct_witnesses);
            let mut right_str = String::new();
            write_expr_dyn(&mut right_str, right, struct_witnesses);

            if *op == SpecBinOp::Shl && left_str.contains(" as ") {
                write!(out, "(({}) << {})", left_str, right_str).unwrap();
            } else {
                write!(out, "({} {} {})", left_str, match op {
                    SpecBinOp::Add => "+",
                    SpecBinOp::Sub => "-",
                    SpecBinOp::Mul => "*",
                    SpecBinOp::Div => "/",
                    SpecBinOp::Rem => "%",
                    SpecBinOp::BitAnd => "&",
                    SpecBinOp::BitOr => "|",
                    SpecBinOp::BitXor => "^",
                    SpecBinOp::Shl => "<<",
                    SpecBinOp::Shr => ">>",
                    SpecBinOp::Eq => "==",
                    SpecBinOp::Ne => "!=",
                    SpecBinOp::Lt => "<",
                    SpecBinOp::Le => "<=",
                    SpecBinOp::Gt => ">",
                    SpecBinOp::Ge => ">=",
                    _ => "+", // Fallback
                }, right_str).unwrap();
            }
        }
        IrExpr::Unary { op, expr } => {
            match op {
                SpecUnaryOp::Neg => write!(out, "-").unwrap(),
                SpecUnaryOp::Not => write!(out, "!").unwrap(),
                SpecUnaryOp::Deref => write!(out, "*").unwrap(),
                SpecUnaryOp::Ref => write!(out, "&").unwrap(),
                SpecUnaryOp::RefMut => write!(out, "&mut ").unwrap(),
            }
            write_expr_dyn(out, expr, struct_witnesses);
        }
        IrExpr::Call { func, args } => {
            if let IrExpr::Var(v) = func.as_ref() {
                if is_likely_len_param(v) {
                    write!(out, "{}", v.to_lowercase()).unwrap();
                } else if v == "ilog2" {
                    write!(out, "ilog2(").unwrap();
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 { write!(out, ", ").unwrap(); }
                        write_expr_dyn(out, arg, struct_witnesses);
                    }
                    write!(out, ")").unwrap();
                } else {
                    write_expr_dyn(out, func, struct_witnesses);
                    write!(out, "(").unwrap();
                    for (i, arg) in args.iter().enumerate() {
                        if i > 0 { write!(out, ", ").unwrap(); }
                        write_expr_dyn(out, arg, struct_witnesses);
                    }
                    write!(out, ")").unwrap();
                }
            } else {
                write_expr_dyn(out, func, struct_witnesses);
                write!(out, "(").unwrap();
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { write!(out, ", ").unwrap(); }
                    write_expr_dyn(out, arg, struct_witnesses);
                }
                write!(out, ")").unwrap();
            }
        }
        IrExpr::MethodCall { receiver, method, args, .. } => {
            let method_name = match method {
                MethodKind::Std(s) => s.clone(),
                MethodKind::Vole(v) => match v {
                     VoleMethod::Remap => "remap".to_string(),
                     VoleMethod::RotateLeft => "rotate_left".to_string(),
                },
                MethodKind::Crypto(c) => format!("{:?}", c).to_lowercase(),
                MethodKind::Unknown(s) => s.clone(),
            };
            
            if method_name == "to_usize" {
                write_expr_dyn(out, receiver, struct_witnesses);
            } else if method_name == "ilog2" {
                write!(out, "ilog2(").unwrap();
                write_expr_dyn(out, receiver, struct_witnesses);
                write!(out, ")").unwrap();
            } else {
                write_expr_dyn(out, receiver, struct_witnesses);
                write!(out, ".{}(", method_name).unwrap();
                for (i, arg) in args.iter().enumerate() {
                    if i > 0 { write!(out, ", ").unwrap(); }
                    write_expr_dyn(out, arg, struct_witnesses);
                }
                write!(out, ")").unwrap();
            }
        }
        IrExpr::Field { base, field } => {
            write_expr_dyn(out, base, struct_witnesses);
            write!(out, ".{}", field).unwrap();
        }
        IrExpr::Index { base, index } => {
            write_expr_dyn(out, base, struct_witnesses);
            write!(out, "[").unwrap();
            write_expr_dyn(out, index, struct_witnesses);
            write!(out, "]").unwrap();
        }
        IrExpr::Block(b) => {
            writeln!(out, "{{").unwrap();
            for stmt in &b.stmts {
                write_stmt_dyn(out, stmt, 1, struct_witnesses);
            }
            if let Some(e) = &b.expr {
                write!(out, "    ").unwrap();
                write_expr_dyn(out, e, struct_witnesses);
                writeln!(out).unwrap();
            }
            write!(out, "}}").unwrap();
        }
        IrExpr::ArrayGenerate { index_var, body, len, .. } => {
            let n = match len {
                ArrayLength::Const(n) => n.to_string(),
                ArrayLength::TypeNum(tn) => tn.to_usize().to_string(),
                ArrayLength::TypeParam(p) => p.to_lowercase(),
                ArrayLength::Computed(e) => {
                    let mut s = String::new();
                    write_expr_dyn(&mut s, e, struct_witnesses);
                    s
                }
            };
            write!(out, "(0..{}).map(|{}| ", n, index_var).unwrap();
            write_expr_dyn(out, body, struct_witnesses);
            write!(out, ").collect()").unwrap();
        }
        IrExpr::ArrayMap { array, elem_var, body } => {
            write_expr_dyn(out, array, struct_witnesses);
            write!(out, ".iter().map(|{}| ", elem_var).unwrap();
            write_expr_dyn(out, body, struct_witnesses);
            write!(out, ").collect()").unwrap();
        }
        IrExpr::ArrayZip { left, right, left_var, right_var, body } => {
            write_expr_dyn(out, left, struct_witnesses);
            write!(out, ".iter().zip(").unwrap();
            write_expr_dyn(out, right, struct_witnesses);
            write!(out, ".iter()).map(|({}, {})| ", left_var, right_var).unwrap();
            write_expr_dyn(out, body, struct_witnesses);
            write!(out, ").collect()").unwrap();
        }
        IrExpr::ArrayFold { array, init, acc_var, elem_var, body } => {
            write_expr_dyn(out, array, struct_witnesses);
            write!(out, ".iter().fold(").unwrap();
            write_expr_dyn(out, init, struct_witnesses);
            write!(out, ", |{}, {}| ", acc_var, elem_var).unwrap();
            write_expr_dyn(out, body, struct_witnesses);
            write!(out, ")").unwrap();
        }
        IrExpr::BoundedLoop { var, start, end, inclusive, body } => {
            write!(out, "for {} in ", var).unwrap();
            write_expr_dyn(out, start, struct_witnesses);
            write!(out, "{} ", if *inclusive { "..=" } else { ".." }).unwrap();
            write_expr_dyn(out, end, struct_witnesses);
            writeln!(out, " {{").unwrap();
            for stmt in &body.stmts {
                write_stmt_dyn(out, stmt, 1, struct_witnesses);
            }
            if let Some(e) = &body.expr {
                write!(out, "    ").unwrap();
                write_expr_dyn(out, e, struct_witnesses);
                writeln!(out).unwrap();
            }
            write!(out, "}}").unwrap();
        }
        IrExpr::If { cond, then_branch, else_branch } => {
            write!(out, "if ").unwrap();
            write_expr_dyn(out, cond, struct_witnesses);
            writeln!(out, " {{").unwrap();
            for stmt in &then_branch.stmts {
                write_stmt_dyn(out, stmt, 1, struct_witnesses);
            }
            if let Some(e) = &then_branch.expr {
                write!(out, "    ").unwrap();
                write_expr_dyn(out, e, struct_witnesses);
                writeln!(out).unwrap();
            }
            write!(out, "}}").unwrap();
            if let Some(eb) = else_branch {
                write!(out, " else ").unwrap();
                write_expr_dyn(out, eb, struct_witnesses);
            }
        }
        IrExpr::Match { expr, arms } => {
            write!(out, "match ").unwrap();
            write_expr_dyn(out, expr, struct_witnesses);
            writeln!(out, " {{").unwrap();
            for arm in arms {
                write!(out, "    ").unwrap();
                write_pattern_dyn(out, &arm.pattern);
                write!(out, " => ").unwrap();
                write_expr_dyn(out, &arm.body, struct_witnesses);
                writeln!(out, ",").unwrap();
            }
            write!(out, "}}").unwrap();
        }
        IrExpr::Return(e) => {
            write!(out, "return").unwrap();
            if let Some(e) = e {
                write!(out, " ").unwrap();
                write_expr_dyn(out, e, struct_witnesses);
            }
        }
        IrExpr::Break(e) => {
            write!(out, "break").unwrap();
            if let Some(e) = e {
                write!(out, " ").unwrap();
                write_expr_dyn(out, e, struct_witnesses);
            }
        }
        IrExpr::Continue => write!(out, "continue").unwrap(),
        IrExpr::Assign { left, right } => {
            write_expr_dyn(out, left, struct_witnesses);
            write!(out, " = ").unwrap();
            write_expr_dyn(out, right, struct_witnesses);
        }
        IrExpr::AssignOp { op, left, right } => {
            write_expr_dyn(out, left, struct_witnesses);
            write!(out, " {} = ", match op {
                SpecBinOp::Add => "+",
                SpecBinOp::Sub => "-",
                SpecBinOp::Mul => "*",
                SpecBinOp::Div => "/",
                SpecBinOp::Rem => "%",
                SpecBinOp::BitAnd => "&",
                SpecBinOp::BitOr => "|",
                SpecBinOp::BitXor => "^",
                SpecBinOp::Shl => "<<",
                SpecBinOp::Shr => ">>",
                _ => "+",
            }).unwrap();
            write_expr_dyn(out, right, struct_witnesses);
        }
        IrExpr::Path { segments, .. } => {
            if segments.len() == 2 && is_likely_len_param(&segments[0]) && segments[1] == "to_usize" {
                write!(out, "{}", segments[0].to_lowercase()).unwrap();
            } else {
                write!(out, "{}", segments.join("::")).unwrap();
            }
        }
        IrExpr::Closure { params, body, .. } => {
            write!(out, "|").unwrap();
            for (i, p) in params.iter().enumerate() {
                if i > 0 { write!(out, ", ").unwrap(); }
                write_pattern_dyn(out, &p.pattern);
            }
            write!(out, "| ").unwrap();
            write_expr_dyn(out, body, struct_witnesses);
        }
        IrExpr::Cast { expr, ty } => {
            write_expr_dyn(out, expr, struct_witnesses);
            write!(out, " as ").unwrap();
            write_type_dyn(out, ty);
        }
        IrExpr::Try(e) => {
            write_expr_dyn(out, e, struct_witnesses);
            write!(out, "?").unwrap();
        }
        IrExpr::Tuple(elems) => {
            write!(out, "(").unwrap();
            for (i, e) in elems.iter().enumerate() {
                if i > 0 { write!(out, ", ").unwrap(); }
                write_expr_dyn(out, e, struct_witnesses);
            }
            write!(out, ")").unwrap();
        }
        IrExpr::Array(elems) => {
            write!(out, "vec![").unwrap();
            for (i, e) in elems.iter().enumerate() {
                if i > 0 { write!(out, ", ").unwrap(); }
                write_expr_dyn(out, e, struct_witnesses);
            }
            write!(out, "]").unwrap();
        }
        IrExpr::StructExpr { kind, fields, .. } => {
            write!(out, "{}Dyn {{ ", kind).unwrap();
            
            let mut field_names: BTreeSet<_> = fields.iter().map(|(n, _)| n.clone()).collect();
            for (i, (name, val)) in fields.iter().enumerate() {
                if i > 0 { write!(out, ", ").unwrap(); }
                write!(out, "{}: ", name).unwrap();
                write_expr_dyn(out, val, struct_witnesses);
            }
            
            if let Some(witnesses) = struct_witnesses.get(&kind.to_string()) {
                for w in witnesses {
                    let w_low = w.to_lowercase();
                    if !field_names.contains(&w_low) {
                        if !fields.is_empty() || !field_names.is_empty() { write!(out, ", ").unwrap(); }
                        write!(out, "{}: {}", w_low, w_low).unwrap();
                        field_names.insert(w_low);
                    }
                }
            }
            
            write!(out, " }}").unwrap();
        }
        _ => write!(out, "todo!()").unwrap(),
    }
}

fn write_pattern_dyn(out: &mut String, pat: &IrPattern) {
    match pat {
        IrPattern::Ident { name, .. } => write!(out, "{}", name).unwrap(),
        IrPattern::Wild => write!(out, "_").unwrap(),
        IrPattern::Tuple(pats) => {
            write!(out, "(").unwrap();
            for (i, p) in pats.iter().enumerate() {
                if i > 0 { write!(out, ", ").unwrap(); }
                write_pattern_dyn(out, p);
            }
            write!(out, ")").unwrap();
        }
        IrPattern::Lit(l) => write!(out, "{}", l).unwrap(),
        _ => write!(out, "_").unwrap(),
    }
}
