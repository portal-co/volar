//! Specialized IR types for volar-spec.
//!
//! This module provides the unified intermediate representation for volar-spec,
//! incorporating domain-specific knowledge about cryptography, math, and VOLE.

use core::fmt;

#[cfg(feature = "std")]
use std::{
    boxed::Box,
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};

#[cfg(not(feature = "std"))]
use alloc::{
    boxed::Box,
    format,
    string::{String, ToString},
    vec,
    vec::Vec,
};

use thiserror::Error;

#[derive(Error, Debug)]
pub enum CompilerError {
    #[error("Parse error: {0}")]
    ParseError(String),
    #[error("Unsupported construct: {0}")]
    Unsupported(String),
    #[error("Specialization error: {0}")]
    SpecializationError(String),
    #[error("Unbounded loop detected: only total (bounded) loops are allowed")]
    UnboundedLoop,
    #[error("Invalid type: {0}")]
    InvalidType(String),
}

/// Iterator source methods — how we obtain an iterator from a collection.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum IterMethod {
    Iter,
    IntoIter,
    Chars,
    Bytes,
}

impl IterMethod {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "iter" => Some(Self::Iter),
            "into_iter" => Some(Self::IntoIter),
            "chars" => Some(Self::Chars),
            "bytes" => Some(Self::Bytes),
            _ => None,
        }
    }
}

// ============================================================================
// ITERATOR CHAIN (flat representation of iterator pipelines)
// ============================================================================

/// A complete iterator pipeline: source → steps → terminal.
///
/// This replaces the old nested `Iter*` / `Array{Map,Zip,Fold}` expression
/// variants with a single flat structure that is easy to analyze and
/// backend-neutral.
#[derive(Debug, Clone, PartialEq)]
pub struct IrIterChain {
    /// Where the data comes from
    pub source: IterChainSource,
    /// Zero or more intermediate transformations, in order
    pub steps: Vec<IterStep>,
    /// How the pipeline terminates
    pub terminal: IterTerminal,
}

/// The data source for an iterator chain.
#[derive(Debug, Clone, PartialEq)]
pub enum IterChainSource {
    /// `expr.iter()`, `expr.into_iter()`, `expr.chars()`, `expr.bytes()`
    Method {
        collection: Box<IrExpr>,
        method: IterMethod,
    },
    /// A range expression used as an iterator: `start..end` or `start..=end`
    Range {
        start: Box<IrExpr>,
        end: Box<IrExpr>,
        inclusive: bool,
    },
    /// Zipping two iterator chains: `a.iter().zip(b.iter())`
    Zip {
        left: Box<IrIterChain>,
        right: Box<IrIterChain>,
    },
}

/// An intermediate transformation step in an iterator pipeline.
#[derive(Debug, Clone, PartialEq)]
pub enum IterStep {
    /// `.map(|var| body)`
    Map { var: IrPattern, body: Box<IrExpr> },
    /// `.filter(|var| body)`
    Filter { var: IrPattern, body: Box<IrExpr> },
    /// `.filter_map(|var| body)`
    FilterMap { var: IrPattern, body: Box<IrExpr> },
    /// `.flat_map(|var| body)`
    FlatMap { var: IrPattern, body: Box<IrExpr> },
    /// `.enumerate()`
    Enumerate,
    /// `.take(count)`
    Take { count: Box<IrExpr> },
    /// `.skip(count)`
    Skip { count: Box<IrExpr> },
    /// `.chain(other)` — appends another iterator chain.
    Chain { other: Box<IrIterChain> },
}

/// How an iterator pipeline terminates.
#[derive(Debug, Clone, PartialEq)]
pub enum IterTerminal {
    /// `.collect()` — materializes into a `Vec` (or other container).
    Collect,
    /// `.collect::<Vec<T>>()` — typed collect for disambiguation
    CollectTyped(IrType),
    /// `.fold(init, |acc, elem| body)`
    Fold {
        init: Box<IrExpr>,
        acc_var: IrPattern,
        elem_var: IrPattern,
        body: Box<IrExpr>,
    },
    /// No terminal — the chain is still lazy (e.g. used as the `collection`
    /// of a `for`-in loop, or passed to another consumer).
    Lazy,
}

pub type Result<T> = core::result::Result<T, CompilerError>;

// ============================================================================
// PRIMITIVE TYPES
// ============================================================================

/// Primitive scalar types used in volar-spec
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum PrimitiveType {
    /// Boolean type
    Bool,
    /// Unsigned 8-bit integer
    U8,
    /// Unsigned 32-bit integer
    U32,
    /// Unsigned 64-bit integer
    U64,
    /// Unsigned pointer-sized integer
    Usize,
    /// Signed 128-bit integer (for literals)
    I128,
    /// Single bit field element
    Bit,
    /// 8-bit Galois field element (GF(2^8) for AES)
    Galois,
    /// 64-bit Galois field element
    Galois64,
    /// 8-bit packed bits (used for SIMD-like operations)
    BitsInBytes,
    /// 64-bit packed bits
    BitsInBytes64,
}

impl PrimitiveType {
    /// Try to parse a primitive type from a string
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "bool" => Some(Self::Bool),
            "u8" => Some(Self::U8),
            "u32" => Some(Self::U32),
            "u64" => Some(Self::U64),
            "usize" => Some(Self::Usize),
            "i128" => Some(Self::I128),
            "Bit" => Some(Self::Bit),
            "Galois" => Some(Self::Galois),
            "Galois64" => Some(Self::Galois64),
            "BitsInBytes" => Some(Self::BitsInBytes),
            "BitsInBytes64" => Some(Self::BitsInBytes64),
            _ => None,
        }
    }

    /// Get the bit width of this type
    pub fn bit_width(&self) -> usize {
        match self {
            Self::Bool | Self::Bit => 1,
            Self::U8 | Self::Galois | Self::BitsInBytes => 8,
            Self::U32 => 32,
            Self::U64 | Self::Galois64 | Self::BitsInBytes64 => 64,
            Self::Usize => std::mem::size_of::<usize>() * 8,
            Self::I128 => 128,
        }
    }

    /// Check if this is a field element type
    pub fn is_field_element(&self) -> bool {
        matches!(
            self,
            Self::Bit | Self::Galois | Self::Galois64 | Self::BitsInBytes | Self::BitsInBytes64
        )
    }
}

impl fmt::Display for PrimitiveType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::U8 => write!(f, "u8"),
            Self::U32 => write!(f, "u32"),
            Self::U64 => write!(f, "u64"),
            Self::Usize => write!(f, "usize"),
            Self::I128 => write!(f, "i128"),
            Self::Bit => write!(f, "Bit"),
            Self::Galois => write!(f, "Galois"),
            Self::Galois64 => write!(f, "Galois64"),
            Self::BitsInBytes => write!(f, "BitsInBytes"),
            Self::BitsInBytes64 => write!(f, "BitsInBytes64"),
        }
    }
}

// ============================================================================
// ARRAY TYPES
// ============================================================================

/// Known array types in volar-spec
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ArrayKind {
    /// `GenericArray<T, N>`
    GenericArray,
    /// `[T; N]`
    FixedArray,
    /// `[T]` slice
    Slice,
}

/// Array length representation
#[derive(Debug, Clone, PartialEq)]
pub enum ArrayLength {
    /// Constant size
    Const(usize),
    /// Typenum constant (U32, etc.)
    TypeNum(TypeNumConst),
    /// Generic type parameter (N, etc.)
    TypeParam(String),

    Projection {
        r#type: Box<IrType>,
        field: String,
        /// Optional trait path for qualified projections like `<T as Logarithm2>::Output`
        trait_path: Option<String>,
    },
}

/// Typenum constants commonly used
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeNumConst {
    U0,
    U1,
    U2,
    U8,
    U16,
    U32,
    U64,
}

impl TypeNumConst {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "U0" => Some(Self::U0),
            "U1" => Some(Self::U1),
            "U2" => Some(Self::U2),
            "U8" => Some(Self::U8),
            "U16" => Some(Self::U16),
            "U32" => Some(Self::U32),
            "U64" => Some(Self::U64),
            _ => None,
        }
    }

    pub fn to_usize(&self) -> usize {
        match self {
            Self::U0 => 0,
            Self::U1 => 1,
            Self::U2 => 2,
            Self::U8 => 8,
            Self::U16 => 16,
            Self::U32 => 32,
            Self::U64 => 64,
        }
    }
}

// ============================================================================
// STRUCT & ENUM KINDS
// ============================================================================

/// Known struct types in volar-spec
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StructKind {
    /// GenericArray — special semantics in lowering (type-level-sized array)
    GenericArray,
    /// Any named struct
    Custom(String),
}

impl Default for StructKind {
    fn default() -> Self {
        Self::Custom("Unknown".to_string())
    }
}

impl StructKind {
    pub fn from_str(s: &str) -> Self {
        match s {
            "GenericArray" => Self::GenericArray,
            other => Self::Custom(other.to_string()),
        }
    }
}

impl fmt::Display for StructKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::GenericArray => write!(f, "GenericArray"),
            Self::Custom(name) => write!(f, "{}", name),
        }
    }
}

// ============================================================================
// TRAIT & METHOD KINDS
// ============================================================================

/// Standard mathematical traits
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MathTrait {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Neg,
    Not,
    PartialEq,
    Eq,
    PartialOrd,
    Ord,
    Clone,
    Copy,
    Default,
    Unsigned,
    Logarithm2,
}

impl MathTrait {
    pub fn from_path(segments: &[String]) -> Option<Self> {
        let name = segments.last()?;
        match name.as_str() {
            "Add" => Some(Self::Add),
            "Sub" => Some(Self::Sub),
            "Mul" => Some(Self::Mul),
            "Div" => Some(Self::Div),
            "Rem" => Some(Self::Rem),
            "BitAnd" => Some(Self::BitAnd),
            "BitOr" => Some(Self::BitOr),
            "BitXor" => Some(Self::BitXor),
            "Shl" => Some(Self::Shl),
            "Shr" => Some(Self::Shr),
            "Neg" => Some(Self::Neg),
            "Not" => Some(Self::Not),
            "PartialEq" => Some(Self::PartialEq),
            "Eq" => Some(Self::Eq),
            "PartialOrd" => Some(Self::PartialOrd),
            "Ord" => Some(Self::Ord),
            "Clone" => Some(Self::Clone),
            "Copy" => Some(Self::Copy),
            "Default" => Some(Self::Default),
            "Unsigned" => Some(Self::Unsigned),
            "Logarithm2" => Some(Self::Logarithm2),
            _ => None,
        }
    }
}

/// Unified trait classification
#[derive(Debug, Clone, PartialEq)]
pub enum TraitKind {
    Math(MathTrait),
    Into(Box<IrType>),
    AsRef(Box<IrType>),
    /// Fn-like trait: input variant and output type
    Fn(FnInput, Box<IrType>),
    External {
        path: Vec<String>,
    },
    Custom(String),
}

impl TraitKind {
    pub fn from_path(segments: &[String]) -> Self {
        if let Some(math) = MathTrait::from_path(segments) {
            return Self::Math(math);
        }
        // Normalize Rng aliases
        let last = segments.last().cloned().unwrap_or_default();
        let name = match last.as_str() {
            "RngCore" | "CryptoRng" => "Rng".to_string(),
            _ => last,
        };
        if segments.len() > 1 {
            return Self::External {
                path: segments.to_vec(),
            };
        }
        Self::Custom(name)
    }
}

impl fmt::Display for TraitKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Math(m) => write!(f, "{:?}", m),
            Self::External { path } => write!(f, "{}", path.join("::")),
            Self::Custom(name) => write!(f, "{}", name),
            Self::Into(ty) => {
                write!(f, "Into<{}>", ty)
            }
            Self::AsRef(ty) => {
                write!(f, "AsRef<{}>", ty)
            }
            Self::Fn(input, ty) => match input {
                FnInput::BytesSlice => write!(f, "FnMut(&[u8]) -> {}", ty),
                FnInput::Size => write!(f, "FnMut(usize) -> {}", ty),
                FnInput::Bool => write!(f, "FnMut(bool) -> {}", ty),
            },
        }
    }
}

/// Input kinds allowed for generated Fn-like traits
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum FnInput {
    BytesSlice,
    Size,
    Bool,
}

/// VOLE-specific method names
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VoleMethod {
    Remap,
    RotateLeft,
}

impl VoleMethod {
    pub fn try_from_str(s: &str) -> Option<Self> {
        match s {
            "remap" => Some(Self::Remap),
            "rotate_left" => Some(Self::RotateLeft),
            _ => None,
        }
    }
}

/// Unified method classification
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MethodKind {
    Vole(VoleMethod),
    Std(String),
    Unknown(String),
}

impl MethodKind {
    pub fn from_str(s: &str) -> Self {
        if let Some(v) = VoleMethod::try_from_str(s) {
            return Self::Vole(v);
        }

        // Common std-like methods
        match s {
            "clone" | "default" | "into" | "from" | "as_ref" | "as_slice" | "get" | "len"
            | "is_empty" | "contains" | "unwrap" | "unwrap_or" | "unwrap_or_default" | "expect"
            | "to_usize" | "to_string" | "as_ptr" | "wrapping_add" | "wrapping_sub"
            | "checked_add" | "checked_sub" | "saturating_add" | "saturating_sub" | "bitxor"
            | "deref" => return Self::Std(s.to_string()),
            _ => {}
        }

        Self::Unknown(s.to_string())
    }
}

/// Associated type names in traits
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub enum AssociatedType {
    Output,
    Key,
    BlockSize,
    OutputSize,
    TotalLoopCount,
    Other(String),
}

impl AssociatedType {
    pub fn from_str(s: &str) -> Self {
        match s {
            "Output" => Self::Output,
            "Key" => Self::Key,
            "BlockSize" => Self::BlockSize,
            "OutputSize" => Self::OutputSize,
            "TotalLoopCount" => Self::TotalLoopCount,
            _ => Self::Other(s.to_string()),
        }
    }
}

impl fmt::Display for AssociatedType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Output => write!(f, "Output"),
            Self::Key => write!(f, "Key"),
            Self::BlockSize => write!(f, "BlockSize"),
            Self::OutputSize => write!(f, "OutputSize"),
            Self::TotalLoopCount => write!(f, "TotalLoopCount"),
            Self::Other(name) => write!(f, "{}", name),
        }
    }
}

// ============================================================================
// MAIN IR DATA STRUCTURES
// ============================================================================

#[derive(Debug, Clone, PartialEq, Default)]
pub struct IrModule {
    pub name: String,
    pub structs: Vec<IrStruct>,
    pub traits: Vec<IrTrait>,
    pub impls: Vec<IrImpl>,
    pub functions: Vec<IrFunction>,
    pub type_aliases: Vec<IrTypeAlias>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrStruct {
    pub kind: StructKind,
    pub generics: Vec<IrGenericParam>,
    pub fields: Vec<IrField>,
    pub is_tuple: bool,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrTrait {
    pub kind: TraitKind,
    pub generics: Vec<IrGenericParam>,
    pub super_traits: Vec<IrTraitBound>,
    pub items: Vec<IrTraitItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrTraitItem {
    Method(IrMethodSig),
    AssociatedType {
        name: AssociatedType,
        bounds: Vec<IrTraitBound>,
        default: Option<IrType>,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrImpl {
    pub generics: Vec<IrGenericParam>,
    pub trait_: Option<IrTraitRef>,
    pub self_ty: IrType,
    pub where_clause: Vec<IrWherePredicate>,
    pub items: Vec<IrImplItem>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrTraitRef {
    pub kind: TraitKind,
    pub type_args: Vec<IrType>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrImplItem {
    Method(IrFunction),
    AssociatedType { name: AssociatedType, ty: IrType },
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrMethodSig {
    pub name: String,
    pub generics: Vec<IrGenericParam>,
    pub receiver: Option<IrReceiver>,
    pub params: Vec<IrParam>,
    pub return_type: Option<IrType>,
    pub where_clause: Vec<IrWherePredicate>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrFunction {
    pub name: String,
    pub generics: Vec<IrGenericParam>,
    pub receiver: Option<IrReceiver>,
    pub params: Vec<IrParam>,
    pub return_type: Option<IrType>,
    pub where_clause: Vec<IrWherePredicate>,
    pub body: IrBlock,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrParam {
    pub name: String,
    pub ty: IrType,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrField {
    pub name: String,
    pub ty: IrType,
    pub public: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum IrGenericParamKind {
    Type,
    Const,
    Lifetime,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrGenericParam {
    pub name: String,
    pub kind: IrGenericParamKind,
    pub bounds: Vec<IrTraitBound>,
    pub default: Option<IrType>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrWherePredicate {
    TypeBound {
        ty: IrType,
        bounds: Vec<IrTraitBound>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IrReceiver {
    Value,
    Ref,
    RefMut,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrTypeAlias {
    pub name: String,
    pub generics: Vec<IrGenericParam>,
    pub target: IrType,
}

#[derive(Debug, Clone, PartialEq, Default)]
pub struct IrBlock {
    pub stmts: Vec<IrStmt>,
    pub expr: Option<Box<IrExpr>>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrStmt {
    Let {
        pattern: IrPattern,
        ty: Option<IrType>,
        init: Option<IrExpr>,
    },
    Semi(IrExpr),
    Expr(IrExpr),
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrMatchArm {
    pub pattern: IrPattern,
    pub guard: Option<IrExpr>,
    pub body: IrExpr,
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrClosureParam {
    pub pattern: IrPattern,
    pub ty: Option<IrType>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrType {
    Primitive(PrimitiveType),
    Array {
        kind: ArrayKind,
        elem: Box<IrType>,
        len: ArrayLength,
    },
    Vector {
        elem: Box<IrType>,
    },
    Struct {
        kind: StructKind,
        type_args: Vec<IrType>,
    },
    TypeParam(String),
    Tuple(Vec<IrType>),
    Unit,
    Reference {
        mutable: bool,
        elem: Box<IrType>,
    },
    Projection {
        base: Box<IrType>,
        /// The trait for the projection (e.g., "BlockEncrypt" for <B as BlockEncrypt>::BlockSize)
        trait_path: Option<String>,
        trait_args: Vec<IrType>,
        assoc: AssociatedType,
    },
    /// Existential type (impl Trait)
    Existential {
        bounds: Vec<IrTraitBound>,
    },
    FnPtr {
        params: Vec<IrType>,
        ret: Box<IrType>,
    },
    Never,
    Infer,
    Param {
        path: Vec<String>,
    },
}

impl fmt::Display for IrType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Primitive(p) => write!(f, "{}", p),
            Self::Array { kind: _, elem, len } => {
                write!(f, "[{}; ", elem)?;
                match len {
                    ArrayLength::Const(n) => write!(f, "{}", n)?,
                    ArrayLength::TypeNum(tn) => write!(f, "{:?}", tn)?,
                    ArrayLength::TypeParam(p) => write!(f, "{}", p)?,
                    ArrayLength::Projection { r#type, field, .. } => {
                        write!(f, "<{}>::{}", r#type, field)?;
                    }
                }
                write!(f, "]")
            }
            Self::Vector { elem } => write!(f, "Vec<{}>", elem),
            Self::Struct { kind, type_args } => {
                write!(f, "{}", kind)?;
                if !type_args.is_empty() {
                    write!(f, "<")?;
                    for (i, arg) in type_args.iter().enumerate() {
                        if i > 0 {
                            write!(f, ", ")?;
                        }
                        write!(f, "{}", arg)?;
                    }
                    write!(f, ">")?;
                }
                Ok(())
            }
            Self::TypeParam(p) => write!(f, "{}", p),
            Self::Tuple(elems) => {
                write!(f, "(")?;
                for (i, elem) in elems.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", elem)?;
                }
                write!(f, ")")
            }
            Self::Unit => write!(f, "()"),
            Self::Reference { mutable, elem } => {
                write!(f, "&{}{}", if *mutable { "mut " } else { "" }, elem)
            }
            Self::Projection {
                base,
                assoc,
                trait_path,
                ..
            } => {
                let trait_name = trait_path.as_deref().unwrap_or("_");
                write!(f, "<{} as {}>::{}", base, trait_name, assoc)
            }
            Self::Existential { bounds } => {
                write!(f, "impl ")?;
                for (i, bound) in bounds.iter().enumerate() {
                    if i > 0 {
                        write!(f, " + ")?;
                    }
                    write!(f, "{}", bound)?; // IrTraitBound has Display impl
                }
                Ok(())
            }
            Self::FnPtr { params, ret } => {
                write!(f, "fn(")?;
                for (i, p) in params.iter().enumerate() {
                    if i > 0 {
                        write!(f, ", ")?;
                    }
                    write!(f, "{}", p)?;
                }
                write!(f, ") -> {}", ret)
            }
            Self::Never => write!(f, "!"),
            Self::Infer => write!(f, "_"),
            Self::Param { path } => write!(f, "{}", path.join("::")),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrTraitBound {
    pub trait_kind: TraitKind,
    pub type_args: Vec<IrType>,
    pub assoc_bindings: Vec<(AssociatedType, IrType)>,
}

impl fmt::Display for IrTraitBound {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.trait_kind)?;
        if !self.type_args.is_empty() || !self.assoc_bindings.is_empty() {
            write!(f, "<")?;
            let mut first = true;
            for arg in &self.type_args {
                if !first {
                    write!(f, ", ")?;
                }
                write!(f, "{}", arg)?;
                first = false;
            }
            for (name, ty) in &self.assoc_bindings {
                if !first {
                    write!(f, ", ")?;
                }
                write!(f, "{} = {}", name, ty)?;
                first = false;
            }
            write!(f, ">")?;
        }
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrExpr {
    Lit(IrLit),
    Var(String),
    Path {
        segments: Vec<String>,
        type_args: Vec<IrType>,
    },
    Binary {
        op: SpecBinOp,
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    Unary {
        op: SpecUnaryOp,
        expr: Box<IrExpr>,
    },
    MethodCall {
        receiver: Box<IrExpr>,
        method: MethodKind,
        type_args: Vec<IrType>,
        args: Vec<IrExpr>,
    },
    Call {
        func: Box<IrExpr>,
        args: Vec<IrExpr>,
    },
    Field {
        base: Box<IrExpr>,
        field: String,
    },
    Index {
        base: Box<IrExpr>,
        index: Box<IrExpr>,
    },
    StructExpr {
        kind: StructKind,
        type_args: Vec<IrType>,
        fields: Vec<(String, IrExpr)>,
        rest: Option<Box<IrExpr>>,
    },
    Tuple(Vec<IrExpr>),
    Array(Vec<IrExpr>),
    Repeat {
        elem: Box<IrExpr>,
        len: Box<IrExpr>,
    },
    ArrayGenerate {
        elem_ty: Option<Box<IrType>>,
        len: ArrayLength,
        index_var: String,
        body: Box<IrExpr>,
    },
    /// `GenericArray::<T, N>::default()` or `Vec::default()` — produces a
    /// zero-filled / default-filled array of the given element type and length.
    /// In dyn lowering this becomes `vec![T::default(); n]`.
    ArrayDefault {
        elem_ty: Option<Box<IrType>>,
        len: ArrayLength,
    },
    /// Calls `T::default()` for a given type. Emitted by dyn lowering when
    /// generating default-filled arrays via IterPipeline. The printer emits
    /// `<ty>::default()` (or `Default::default()` if `ty` is `None`).
    DefaultValue {
        ty: Option<Box<IrType>>,
    },
    /// Witness expression that converts a type-level length to a runtime `usize`.
    /// For simple length params like `N`, lowering produces `Var("n")`.
    /// For projections like `<B as LengthDoubler>::OutputSize`, lowering produces
    /// a runtime expression that reads the length from context.
    LengthOf(ArrayLength),
    /// A flat iterator pipeline (source → steps → terminal).
    /// Replaces old nested Iter*/Array{Map,Zip,Fold} variants.
    IterPipeline(IrIterChain),

    /// Non-iterator element-wise map: `receiver.map(|var| body)`
    /// Used for GenericArray::map and [T; N]::map.
    /// Length-preserving; bounded by the receiver's length.
    RawMap {
        receiver: Box<IrExpr>,
        elem_var: IrPattern,
        body: Box<IrExpr>,
    },

    /// Non-iterator element-wise zip-with-map: `receiver.zip(other, |a, b| body)`
    /// Used for GenericArray::zip. Length-preserving; bounded.
    RawZip {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        left_var: IrPattern,
        right_var: IrPattern,
        body: Box<IrExpr>,
    },

    /// Non-iterator fold over array: `receiver.fold(init, |acc, elem| body)`
    /// When applied directly on GenericArray/[T;N] (no .iter() prefix).
    RawFold {
        receiver: Box<IrExpr>,
        init: Box<IrExpr>,
        acc_var: IrPattern,
        elem_var: IrPattern,
        body: Box<IrExpr>,
    },

    BoundedLoop {
        var: String,
        start: Box<IrExpr>,
        end: Box<IrExpr>,
        inclusive: bool,
        body: IrBlock,
    },
    IterLoop {
        pattern: IrPattern,
        collection: Box<IrExpr>,
        body: IrBlock,
    },
    Block(IrBlock),
    If {
        cond: Box<IrExpr>,
        then_branch: IrBlock,
        else_branch: Option<Box<IrExpr>>,
    },
    Match {
        expr: Box<IrExpr>,
        arms: Vec<IrMatchArm>,
    },
    Closure {
        params: Vec<IrClosureParam>,
        ret_type: Option<Box<IrType>>,
        body: Box<IrExpr>,
    },
    Cast {
        expr: Box<IrExpr>,
        ty: Box<IrType>,
    },
    Return(Option<Box<IrExpr>>),
    Break(Option<Box<IrExpr>>),
    Continue,
    Assign {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    AssignOp {
        op: SpecBinOp,
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    Range {
        start: Option<Box<IrExpr>>,
        end: Option<Box<IrExpr>>,
        inclusive: bool,
    },
    TypenumUsize {
        ty: Box<IrType>,
    },
    Unreachable,
    Try(Box<IrExpr>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrPattern {
    Ident {
        mutable: bool,
        name: String,
        subpat: Option<Box<IrPattern>>,
    },
    Tuple(Vec<IrPattern>),
    Struct {
        kind: StructKind,
        fields: Vec<(String, IrPattern)>,
        rest: bool,
    },
    TupleStruct {
        kind: StructKind,
        elems: Vec<IrPattern>,
    },
    Slice(Vec<IrPattern>),
    Wild,
    Lit(IrLit),
    Ref {
        mutable: bool,
        pat: Box<IrPattern>,
    },
    Or(Vec<IrPattern>),
    Rest,
}

impl IrPattern {
    /// Convenience: create an `Ident` pattern from a string.
    pub fn ident(name: impl Into<String>) -> Self {
        IrPattern::Ident {
            mutable: false,
            name: name.into(),
            subpat: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum IrLit {
    Int(i128),
    Float(f64),
    Bool(bool),
    Char(char),
    Str(String),
    ByteStr(Vec<u8>),
    Byte(u8),
    Unit,
}

impl IrLit {
    pub fn to_string(&self) -> String {
        match self {
            Self::Int(n) => n.to_string(),
            Self::Float(f) => f.to_string(),
            Self::Bool(b) => b.to_string(),
            Self::Char(c) => format!("'{}'", c),
            Self::Str(s) => format!("\"{}\"", s),
            Self::ByteStr(bs) => format!("b\"{:?}\"", bs),
            Self::Byte(b) => format!("b'{}'", b),
            Self::Unit => "()".to_string(),
        }
    }
}

impl fmt::Display for IrLit {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.to_string())
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SpecBinOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    And,
    Or,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SpecUnaryOp {
    Neg,
    Not,
    Deref,
    Ref,
    RefMut,
}

// ============================================================================
// OPERATOR ↔ TRAIT MAPPINGS
// ============================================================================

impl SpecBinOp {
    /// Map a binary operator to its corresponding math trait.
    pub fn to_math_trait(&self) -> Option<MathTrait> {
        match self {
            Self::Add => Some(MathTrait::Add),
            Self::Sub => Some(MathTrait::Sub),
            Self::Mul => Some(MathTrait::Mul),
            Self::Div => Some(MathTrait::Div),
            Self::Rem => Some(MathTrait::Rem),
            Self::BitAnd => Some(MathTrait::BitAnd),
            Self::BitOr => Some(MathTrait::BitOr),
            Self::BitXor => Some(MathTrait::BitXor),
            Self::Shl => Some(MathTrait::Shl),
            Self::Shr => Some(MathTrait::Shr),
            Self::Eq | Self::Ne => Some(MathTrait::PartialEq),
            Self::Lt | Self::Le | Self::Gt | Self::Ge => Some(MathTrait::PartialOrd),
            _ => None,
        }
    }
}

impl MathTrait {
    /// Map a math trait to its canonical binary operator (for backends).
    pub fn to_bin_op(&self) -> Option<SpecBinOp> {
        match self {
            Self::Add => Some(SpecBinOp::Add),
            Self::Sub => Some(SpecBinOp::Sub),
            Self::Mul => Some(SpecBinOp::Mul),
            Self::Div => Some(SpecBinOp::Div),
            Self::Rem => Some(SpecBinOp::Rem),
            Self::BitAnd => Some(SpecBinOp::BitAnd),
            Self::BitOr => Some(SpecBinOp::BitOr),
            Self::BitXor => Some(SpecBinOp::BitXor),
            Self::Shl => Some(SpecBinOp::Shl),
            Self::Shr => Some(SpecBinOp::Shr),
            _ => None,
        }
    }

    /// Method name for this trait (e.g., "add" for Add, "bitxor" for BitXor).
    pub fn method_name(&self) -> Option<&'static str> {
        match self {
            Self::Add => Some("add"),
            Self::Sub => Some("sub"),
            Self::Mul => Some("mul"),
            Self::Div => Some("div"),
            Self::Rem => Some("rem"),
            Self::BitAnd => Some("bitand"),
            Self::BitOr => Some("bitor"),
            Self::BitXor => Some("bitxor"),
            Self::Shl => Some("shl"),
            Self::Shr => Some("shr"),
            Self::Neg => Some("neg"),
            Self::Not => Some("not"),
            Self::PartialEq => Some("eq"),
            Self::PartialOrd => Some("partial_cmp"),
            Self::Clone => Some("clone"),
            Self::Default => Some("default"),
            _ => None,
        }
    }
}

impl SpecUnaryOp {
    /// Map a unary operator to its corresponding math trait.
    pub fn to_math_trait(&self) -> Option<MathTrait> {
        match self {
            Self::Neg => Some(MathTrait::Neg),
            Self::Not => Some(MathTrait::Not),
            _ => None,
        }
    }
}

// ============================================================================
// BUILT-IN TRAIT DEFINITIONS
// ============================================================================

/// Helper to build a binary operator trait definition (Add, Sub, Mul, etc.).
///
/// All binary op traits share the same shape:
/// ```ignore
/// trait Op<Rhs = Self> {
///     type Output;
///     fn op(self, rhs: Rhs) -> Self::Output;
/// }
/// ```
fn builtin_binop_trait(math: MathTrait) -> IrTrait {
    let trait_name = format!("{:?}", math);
    let method = math.method_name().unwrap_or("op");
    IrTrait {
        kind: TraitKind::Math(math),
        generics: vec![IrGenericParam {
            name: "Rhs".into(),
            kind: IrGenericParamKind::Type,
            bounds: vec![],
            default: Some(IrType::TypeParam("Self".into())),
        }],
        super_traits: vec![],
        items: vec![
            IrTraitItem::AssociatedType {
                name: AssociatedType::Output,
                bounds: vec![],
                default: None,
            },
            IrTraitItem::Method(IrMethodSig {
                name: method.into(),
                generics: vec![],
                receiver: Some(IrReceiver::Value),
                params: vec![IrParam {
                    name: "rhs".into(),
                    ty: IrType::TypeParam("Rhs".into()),
                }],
                return_type: Some(IrType::Projection {
                    base: Box::new(IrType::TypeParam("Self".into())),
                    trait_path: Some(trait_name),
                    trait_args: vec![],
                    assoc: AssociatedType::Output,
                }),
                where_clause: vec![],
            }),
        ],
    }
}

/// Helper to build a unary operator trait definition (Neg, Not).
///
/// ```ignore
/// trait Op {
///     type Output;
///     fn op(self) -> Self::Output;
/// }
/// ```
fn builtin_unary_trait(math: MathTrait) -> IrTrait {
    let trait_name = format!("{:?}", math);
    let method = math.method_name().unwrap_or("op");
    IrTrait {
        kind: TraitKind::Math(math),
        generics: vec![],
        super_traits: vec![],
        items: vec![
            IrTraitItem::AssociatedType {
                name: AssociatedType::Output,
                bounds: vec![],
                default: None,
            },
            IrTraitItem::Method(IrMethodSig {
                name: method.into(),
                generics: vec![],
                receiver: Some(IrReceiver::Value),
                params: vec![],
                return_type: Some(IrType::Projection {
                    base: Box::new(IrType::TypeParam("Self".into())),
                    trait_path: Some(trait_name),
                    trait_args: vec![],
                    assoc: AssociatedType::Output,
                }),
                where_clause: vec![],
            }),
        ],
    }
}

/// Canonical definitions for all built-in traits recognized by the IR.
///
/// These provide the single source of truth for trait structure (generics,
/// associated types, methods) so that backends can query them uniformly
/// without hardcoding.
pub fn builtin_trait_defs() -> Vec<IrTrait> {
    vec![
        // Binary operator traits: Add, Sub, Mul, Div, Rem, BitAnd, BitOr, BitXor, Shl, Shr
        builtin_binop_trait(MathTrait::Add),
        builtin_binop_trait(MathTrait::Sub),
        builtin_binop_trait(MathTrait::Mul),
        builtin_binop_trait(MathTrait::Div),
        builtin_binop_trait(MathTrait::Rem),
        builtin_binop_trait(MathTrait::BitAnd),
        builtin_binop_trait(MathTrait::BitOr),
        builtin_binop_trait(MathTrait::BitXor),
        builtin_binop_trait(MathTrait::Shl),
        builtin_binop_trait(MathTrait::Shr),
        // Unary operator traits: Neg, Not
        builtin_unary_trait(MathTrait::Neg),
        builtin_unary_trait(MathTrait::Not),
        // PartialEq<Rhs = Self>
        IrTrait {
            kind: TraitKind::Math(MathTrait::PartialEq),
            generics: vec![IrGenericParam {
                name: "Rhs".into(),
                kind: IrGenericParamKind::Type,
                bounds: vec![],
                default: Some(IrType::TypeParam("Self".into())),
            }],
            super_traits: vec![],
            items: vec![IrTraitItem::Method(IrMethodSig {
                name: "eq".into(),
                generics: vec![],
                receiver: Some(IrReceiver::Ref),
                params: vec![IrParam {
                    name: "other".into(),
                    ty: IrType::Reference {
                        mutable: false,
                        elem: Box::new(IrType::TypeParam("Rhs".into())),
                    },
                }],
                return_type: Some(IrType::Primitive(PrimitiveType::Bool)),
                where_clause: vec![],
            })],
        },
        // Eq: marker trait, supertrait PartialEq
        IrTrait {
            kind: TraitKind::Math(MathTrait::Eq),
            generics: vec![],
            super_traits: vec![IrTraitBound {
                trait_kind: TraitKind::Math(MathTrait::PartialEq),
                type_args: vec![],
                assoc_bindings: vec![],
            }],
            items: vec![],
        },
        // PartialOrd<Rhs = Self>: supertrait PartialEq
        IrTrait {
            kind: TraitKind::Math(MathTrait::PartialOrd),
            generics: vec![IrGenericParam {
                name: "Rhs".into(),
                kind: IrGenericParamKind::Type,
                bounds: vec![],
                default: Some(IrType::TypeParam("Self".into())),
            }],
            super_traits: vec![IrTraitBound {
                trait_kind: TraitKind::Math(MathTrait::PartialEq),
                type_args: vec![IrType::TypeParam("Rhs".into())],
                assoc_bindings: vec![],
            }],
            items: vec![IrTraitItem::Method(IrMethodSig {
                name: "partial_cmp".into(),
                generics: vec![],
                receiver: Some(IrReceiver::Ref),
                params: vec![IrParam {
                    name: "other".into(),
                    ty: IrType::Reference {
                        mutable: false,
                        elem: Box::new(IrType::TypeParam("Rhs".into())),
                    },
                }],
                return_type: Some(IrType::Struct {
                    kind: StructKind::Custom("Option".into()),
                    type_args: vec![IrType::Struct {
                        kind: StructKind::Custom("Ordering".into()),
                        type_args: vec![],
                    }],
                }),
                where_clause: vec![],
            })],
        },
        // Ord: supertrait PartialOrd + Eq
        IrTrait {
            kind: TraitKind::Math(MathTrait::Ord),
            generics: vec![],
            super_traits: vec![
                IrTraitBound {
                    trait_kind: TraitKind::Math(MathTrait::Eq),
                    type_args: vec![],
                    assoc_bindings: vec![],
                },
                IrTraitBound {
                    trait_kind: TraitKind::Math(MathTrait::PartialOrd),
                    type_args: vec![],
                    assoc_bindings: vec![],
                },
            ],
            items: vec![IrTraitItem::Method(IrMethodSig {
                name: "cmp".into(),
                generics: vec![],
                receiver: Some(IrReceiver::Ref),
                params: vec![IrParam {
                    name: "other".into(),
                    ty: IrType::Reference {
                        mutable: false,
                        elem: Box::new(IrType::TypeParam("Self".into())),
                    },
                }],
                return_type: Some(IrType::Struct {
                    kind: StructKind::Custom("Ordering".into()),
                    type_args: vec![],
                }),
                where_clause: vec![],
            })],
        },
        // Clone
        IrTrait {
            kind: TraitKind::Math(MathTrait::Clone),
            generics: vec![],
            super_traits: vec![],
            items: vec![IrTraitItem::Method(IrMethodSig {
                name: "clone".into(),
                generics: vec![],
                receiver: Some(IrReceiver::Ref),
                params: vec![],
                return_type: Some(IrType::TypeParam("Self".into())),
                where_clause: vec![],
            })],
        },
        // Copy: marker trait, supertrait Clone
        IrTrait {
            kind: TraitKind::Math(MathTrait::Copy),
            generics: vec![],
            super_traits: vec![IrTraitBound {
                trait_kind: TraitKind::Math(MathTrait::Clone),
                type_args: vec![],
                assoc_bindings: vec![],
            }],
            items: vec![],
        },
        // Default — static method, no receiver
        IrTrait {
            kind: TraitKind::Math(MathTrait::Default),
            generics: vec![],
            super_traits: vec![],
            items: vec![IrTraitItem::Method(IrMethodSig {
                name: "default".into(),
                generics: vec![],
                receiver: None,
                params: vec![],
                return_type: Some(IrType::TypeParam("Self".into())),
                where_clause: vec![],
            })],
        },
        // Unsigned — marker trait for typenum
        IrTrait {
            kind: TraitKind::Math(MathTrait::Unsigned),
            generics: vec![],
            super_traits: vec![],
            items: vec![],
        },
        // --- Crypto traits ---
        // BlockEncrypt
        IrTrait {
            kind: TraitKind::Custom("BlockEncrypt".into()),
            generics: vec![],
            super_traits: vec![],
            items: vec![
                IrTraitItem::AssociatedType {
                    name: AssociatedType::BlockSize,
                    bounds: vec![],
                    default: None,
                },
                IrTraitItem::Method(IrMethodSig {
                    name: "encrypt_block".into(),
                    generics: vec![],
                    receiver: Some(IrReceiver::Ref),
                    params: vec![IrParam {
                        name: "block".into(),
                        ty: IrType::Reference {
                            mutable: true,
                            elem: Box::new(IrType::Struct {
                                kind: StructKind::Custom("Block".into()),
                                type_args: vec![IrType::TypeParam("Self".into())],
                            }),
                        },
                    }],
                    return_type: None,
                    where_clause: vec![],
                }),
            ],
        },
        // BlockCipher
        IrTrait {
            kind: TraitKind::Custom("BlockCipher".into()),
            generics: vec![],
            super_traits: vec![],
            items: vec![IrTraitItem::AssociatedType {
                name: AssociatedType::BlockSize,
                bounds: vec![],
                default: None,
            }],
        },
        // Digest
        IrTrait {
            kind: TraitKind::Custom("Digest".into()),
            generics: vec![],
            super_traits: vec![],
            items: vec![
                IrTraitItem::AssociatedType {
                    name: AssociatedType::OutputSize,
                    bounds: vec![],
                    default: None,
                },
                IrTraitItem::Method(IrMethodSig {
                    name: "new".into(),
                    generics: vec![],
                    receiver: None,
                    params: vec![],
                    return_type: Some(IrType::TypeParam("Self".into())),
                    where_clause: vec![],
                }),
                IrTraitItem::Method(IrMethodSig {
                    name: "update".into(),
                    generics: vec![],
                    receiver: Some(IrReceiver::RefMut),
                    params: vec![IrParam {
                        name: "data".into(),
                        ty: IrType::Reference {
                            mutable: false,
                            elem: Box::new(IrType::Array {
                                kind: ArrayKind::Slice,
                                elem: Box::new(IrType::Primitive(PrimitiveType::U8)),
                                len: ArrayLength::Const(0),
                            }),
                        },
                    }],
                    return_type: None,
                    where_clause: vec![],
                }),
                IrTraitItem::Method(IrMethodSig {
                    name: "finalize".into(),
                    generics: vec![],
                    receiver: Some(IrReceiver::Value),
                    params: vec![],
                    return_type: Some(IrType::Array {
                        kind: ArrayKind::GenericArray,
                        elem: Box::new(IrType::Primitive(PrimitiveType::U8)),
                        len: ArrayLength::Projection {
                            r#type: Box::new(IrType::TypeParam("Self".into())),
                            field: "OutputSize".into(),
                            trait_path: None,
                        },
                    }),
                    where_clause: vec![],
                }),
            ],
        },
        // ArrayLength<T> — marker/type-level constant
        IrTrait {
            kind: TraitKind::Custom("ArrayLength".into()),
            generics: vec![IrGenericParam {
                name: "T".into(),
                kind: IrGenericParamKind::Type,
                bounds: vec![],
                default: None,
            }],
            super_traits: vec![],
            items: vec![],
        },
        // Rng — minimal definition
        IrTrait {
            kind: TraitKind::Custom("Rng".into()),
            generics: vec![],
            super_traits: vec![],
            items: vec![],
        },
    ]
}
