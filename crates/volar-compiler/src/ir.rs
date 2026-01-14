//! Specialized IR types for volar-spec.
//!
//! This module provides the unified intermediate representation for volar-spec,
//! incorporating domain-specific knowledge about cryptography, math, and VOLE.

use core::fmt;

#[cfg(feature = "std")]
use std::{string::{String, ToString}, vec::Vec, boxed::Box, format};

#[cfg(not(feature = "std"))]
use alloc::{string::{String, ToString}, vec::Vec, boxed::Box, format};

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
    /// Computed expression
    Computed(Box<IrExpr>),
}

/// Typenum constants commonly used
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum TypeNumConst {
    U0, U1, U2, U8, U16, U32, U64,
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
    /// VOLE Delta
    Delta,
    /// VOLE Q
    Q,
    /// VOLE Vope (Vole Opaque)
    Vope,
    /// VOLE BitVole
    BitVole,
    /// Cryptographic Commitment
    ABO,
    /// ABO Opening
    ABOOpening,
    /// Core commitment data
    CommitmentCore,
    /// Polynomial
    Poly,
    /// Input pool for polynomial evaluation
    PolyInputPool,
    /// GenericArray (used as a struct kind sometimes)
    GenericArray,
    /// A custom struct
    Custom(String),
}

impl StructKind {
    pub fn from_str(s: &str) -> Self {
        match s {
            "Delta" => Self::Delta,
            "Q" => Self::Q,
            "Vope" => Self::Vope,
            "BitVole" => Self::BitVole,
            "ABO" => Self::ABO,
            "ABOOpening" => Self::ABOOpening,
            "CommitmentCore" => Self::CommitmentCore,
            "Poly" => Self::Poly,
            "PolyInputPool" => Self::PolyInputPool,
            "GenericArray" => Self::GenericArray,
            other => Self::Custom(other.to_string()),
        }
    }
}

impl fmt::Display for StructKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Delta => write!(f, "Delta"),
            Self::Q => write!(f, "Q"),
            Self::Vope => write!(f, "Vope"),
            Self::BitVole => write!(f, "BitVole"),
            Self::ABO => write!(f, "ABO"),
            Self::ABOOpening => write!(f, "ABOOpening"),
            Self::CommitmentCore => write!(f, "CommitmentCore"),
            Self::Poly => write!(f, "Poly"),
            Self::PolyInputPool => write!(f, "PolyInputPool"),
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
    Add, Sub, Mul, Div, Rem,
    BitAnd, BitOr, BitXor, Shl, Shr,
    Neg, Not,
    PartialEq, Eq, PartialOrd, Ord,
    Clone, Copy, Default,
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
            _ => None,
        }
    }
}

/// Cryptographic and domain-specific traits
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CryptoTrait {
    /// Symmetric encryption block
    BlockEncrypt,
    /// Symmetric cipher
    BlockCipher,
    /// Cryptographic digest
    Digest,
    /// Array length tag
    ArrayLength,
    /// Array specialized for VOLE
    VoleArray,
    /// Encryption on byte blocks
    ByteBlockEncrypt,
}

impl CryptoTrait {
    pub fn from_path(segments: &[String]) -> Option<Self> {
        let name = segments.last()?;
        match name.as_str() {
            "BlockEncrypt" => Some(Self::BlockEncrypt),
            "BlockCipher" => Some(Self::BlockCipher),
            "Digest" => Some(Self::Digest),
            "ArrayLength" => Some(Self::ArrayLength),
            "VoleArray" => Some(Self::VoleArray),
            "ByteBlockEncrypt" => Some(Self::ByteBlockEncrypt),
            _ => None,
        }
    }
}

/// Unified trait classification
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TraitKind {
    Math(MathTrait),
    Crypto(CryptoTrait),
    External { path: Vec<String> },
    Custom(String),
}

impl TraitKind {
    pub fn from_path(segments: &[String]) -> Self {
        if let Some(math) = MathTrait::from_path(segments) {
            return Self::Math(math);
        }
        if let Some(crypto) = CryptoTrait::from_path(segments) {
            return Self::Crypto(crypto);
        }
        if segments.len() > 1 {
            return Self::External { path: segments.to_vec() };
        }
        Self::Custom(segments.last().cloned().unwrap_or_default())
    }
}

impl fmt::Display for TraitKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Math(m) => write!(f, "{:?}", m),
            Self::Crypto(c) => write!(f, "{:?}", c),
            Self::External { path } => write!(f, "{}", path.join("::")),
            Self::Custom(name) => write!(f, "{}", name),
        }
    }
}

/// VOLE-specific method names
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum VoleMethod {
    Remap, RotateLeft,
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

/// Cryptographic method names
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CryptoMethod {
    EncryptBlock, GenAbo, Open, Validate, Commit, Update, Finalize,
}

impl CryptoMethod {
    pub fn try_from_str(s: &str) -> Option<Self> {
        match s {
            "encrypt_block" => Some(Self::EncryptBlock),
            "gen_abo" => Some(Self::GenAbo),
            "open" => Some(Self::Open),
            "validate" => Some(Self::Validate),
            "commit" => Some(Self::Commit),
            "update" => Some(Self::Update),
            "finalize" => Some(Self::Finalize),
            _ => None,
        }
    }
}

/// Unified method classification
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MethodKind {
    Vole(VoleMethod),
    Crypto(CryptoMethod),
    Std(String),
    Unknown(String),
}

impl MethodKind {
    pub fn from_str(s: &str) -> Self {
        if let Some(v) = VoleMethod::try_from_str(s) { return Self::Vole(v); }
        if let Some(c) = CryptoMethod::try_from_str(s) { return Self::Crypto(c); }
        
        match s {
            "clone" | "default" | "into" | "from" | "as_ref" | "as_slice" | "get" | "len"
            | "is_empty" | "contains" | "unwrap" | "unwrap_or" | "unwrap_or_default" | "expect"
            | "to_usize" | "to_string"
            | "as_ptr" | "wrapping_add" | "wrapping_sub" | "checked_add" | "checked_sub"
            | "saturating_add" | "saturating_sub" | "bitxor" | "deref" | "map" | "fold" | "iter" => {
                Self::Std(s.to_string())
            }
            _ => Self::Unknown(s.to_string()),
        }
    }
}

/// Associated type names in traits
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
    pub uses: Vec<String>,
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
    Struct {
        kind: StructKind,
        type_args: Vec<IrType>,
    },
    TypeParam(String),
    Tuple(Vec<IrType>),
    Unit,
    Reference { mutable: bool, elem: Box<IrType> },
    Projection {
        base: Box<IrType>,
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
}

#[derive(Debug, Clone, PartialEq)]
pub struct IrTraitBound {
    pub trait_kind: TraitKind,
    pub type_args: Vec<IrType>,
    pub assoc_bindings: Vec<(AssociatedType, IrType)>,
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
    ArrayMap {
        array: Box<IrExpr>,
        elem_var: String,
        body: Box<IrExpr>,
    },
    ArrayZip {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
        left_var: String,
        right_var: String,
        body: Box<IrExpr>,
    },
    ArrayFold {
        array: Box<IrExpr>,
        init: Box<IrExpr>,
        acc_var: String,
        elem_var: String,
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
    Macro {
        name: String,
        tokens: String,
    },
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SpecBinOp {
    Add, Sub, Mul, Div, Rem,
    BitAnd, BitOr, BitXor, Shl, Shr,
    Eq, Ne, Lt, Le, Gt, Ge,
    And, Or,
    Range, RangeInclusive,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SpecUnaryOp {
    Neg, Not, Deref, Ref, RefMut,
}
