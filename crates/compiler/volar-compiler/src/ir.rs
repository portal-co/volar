// @reliability: normal
// @ai: assisted
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
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum IterMethod {
    Iter,
    IntoIter,
    Chars,
    Bytes,
    /// `.flatten()` — flattens nested iterables (Vec<Vec<T>> → Vec<T>).
    /// In TS, emitted as `.flat()` on the source collection.
    Flatten,
}

impl IterMethod {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "iter" => Some(Self::Iter),
            "into_iter" => Some(Self::IntoIter),
            "chars" => Some(Self::Chars),
            "bytes" => Some(Self::Bytes),
            "flatten" => Some(Self::Flatten),
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
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrIterChain<P: Clone + Default = ()> {
    /// Where the data comes from
    pub source: IterChainSource<P>,
    /// Zero or more intermediate transformations, in order
    pub steps: Vec<IterStep<P>>,
    /// How the pipeline terminates
    pub terminal: IterTerminal<P>,
}

/// The data source for an iterator chain.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum IterChainSource<P: Clone + Default = ()> {
    /// `expr.iter()`, `expr.into_iter()`, `expr.chars()`, `expr.bytes()`
    Method {
        collection: Box<IrExpr<P>>,
        method: IterMethod,
    },
    /// A range expression used as an iterator: `start..end` or `start..=end`
    Range {
        start: Box<IrExpr<P>>,
        end: Box<IrExpr<P>>,
        inclusive: bool,
    },
    /// Zipping two iterator chains: `a.iter().zip(b.iter())`
    Zip {
        left: Box<IrIterChain<P>>,
        right: Box<IrIterChain<P>>,
    },
}

/// An intermediate transformation step in an iterator pipeline.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum IterStep<P: Clone + Default = ()> {
    /// `.map(|var| body)`
    Map { var: IrPattern, body: Box<IrExpr<P>> },
    /// `.filter(|var| body)`
    Filter { var: IrPattern, body: Box<IrExpr<P>> },
    /// `.filter_map(|var| body)`
    FilterMap { var: IrPattern, body: Box<IrExpr<P>> },
    /// `.flat_map(|var| body)`
    FlatMap { var: IrPattern, body: Box<IrExpr<P>> },
    /// `.enumerate()`
    Enumerate,
    /// `.take(count)`
    Take { count: Box<IrExpr<P>> },
    /// `.skip(count)`
    Skip { count: Box<IrExpr<P>> },
    /// `.chain(other)` — appends another iterator chain.
    Chain { other: Box<IrIterChain<P>> },
}

/// How an iterator pipeline terminates.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum IterTerminal<P: Clone + Default = ()> {
    /// `.collect()` — materializes into a `Vec` (or other container).
    Collect,
    /// `.collect::<Vec<T>>()` — typed collect for disambiguation
    CollectTyped(IrType),
    /// `.fold(init, |acc, elem| body)`
    Fold {
        init: Box<IrExpr<P>>,
        acc_var: IrPattern,
        elem_var: IrPattern,
        body: Box<IrExpr<P>>,
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
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
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
    /// Unsigned 128-bit integer
    U128,
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
    /// 128-bit Galois field element (GF(2^128), GCM polynomial)
    Galois128,
    /// 256-bit Galois field element (GF(2^256))
    Galois256,
    /// GF(3) element — mod-3 integer stored in 2 bits.
    ///
    /// Valid only in the TFHE backend; raises an error if passed to
    /// `lower_ir_to_boolar` (which is GF(2)-only).
    Z3,
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
            "u128" => Some(Self::U128),
            "Bit" => Some(Self::Bit),
            "Galois" => Some(Self::Galois),
            "Galois64" => Some(Self::Galois64),
            "Galois128" => Some(Self::Galois128),
            "Galois256" => Some(Self::Galois256),
            "BitsInBytes" => Some(Self::BitsInBytes),
            "BitsInBytes64" => Some(Self::BitsInBytes64),
            "Z3" => Some(Self::Z3),
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
            Self::I128 | Self::U128 | Self::Galois128 => 128,
            Self::Galois256 => 256,
            Self::Z3 => 2,
        }
    }

    /// Check if this is a field element type
    pub fn is_field_element(&self) -> bool {
        matches!(
            self,
            Self::Bit | Self::Galois | Self::Galois64 | Self::Galois128 | Self::Galois256
            | Self::BitsInBytes | Self::BitsInBytes64 | Self::Z3
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
            Self::U128 => write!(f, "u128"),
            Self::Bit => write!(f, "Bit"),
            Self::Galois => write!(f, "Galois"),
            Self::Galois64 => write!(f, "Galois64"),
            Self::Galois128 => write!(f, "Galois128"),
            Self::Galois256 => write!(f, "Galois256"),
            Self::BitsInBytes => write!(f, "BitsInBytes"),
            Self::BitsInBytes64 => write!(f, "BitsInBytes64"),
            Self::Z3 => write!(f, "Z3"),
        }
    }
}

// ============================================================================
// ARRAY TYPES
// ============================================================================

/// Known array types in volar-spec
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
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
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
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
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
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
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
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
            "GenericArray" | "Array" => Self::GenericArray,
            other => Self::Custom(other.to_string()),
        }
    }
}

impl fmt::Display for StructKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::GenericArray => write!(f, "Array"),
            Self::Custom(name) => write!(f, "{}", name),
        }
    }
}

// ============================================================================
// TRAIT & METHOD KINDS
// ============================================================================

/// Standard mathematical traits
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
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

/// Unified trait classification.
///
/// Well-known crypto/stdlib traits that the compiler has specific handling for
/// are named variants.  Everything else goes in `Custom(String)`.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum TraitKind {
    Math(MathTrait),
    Into(Box<IrType>),
    AsRef(Box<IrType>),
    /// Fn-like trait: input variant and output type
    Fn(FnInput, Box<IrType>),
    // ---- Crypto / typenum traits that processing code knows about ----
    /// `generic_array::ArrayLength<T>` — type-level array length marker
    ArrayLength,
    /// `typenum::ArraySize` — alternative length-marker trait  
    ArraySize,
    /// `volar-spec` VOLE array marker trait
    VoleArray,
    /// `volar-common` length-doubling PRG trait
    LengthDoubler,
    /// `cipher::BlockEncrypt`
    BlockEncrypt,
    /// `cipher::BlockCipher`
    BlockCipher,
    /// `digest::Digest`
    Digest,
    /// `rand::RngCore` / `CryptoRng` (normalised to a single variant)
    Rng,
    External {
        path: Vec<String>,
    },
    /// Open-ended user-defined traits not specifically handled by the compiler.
    Custom(String),
}

impl TraitKind {
    /// Asserts in debug builds that this `Custom` variant does not wrap a name
    /// that belongs in a typed [`TraitKind`] variant.
    ///
    /// Call this at every `TraitKind::Custom` code-generation site so that
    /// misrouted construction is caught during testing.
    ///
    /// Note: for `TraitKind`, misrouting is less likely to cause a wrong
    /// *output* (the `Display` impl renders both `Custom("Digest")` and
    /// `Digest` as `"Digest"`), but fixing it early keeps construction sites
    /// honest and is required by the AGENTS.md compiler design rules.
    #[inline]
    pub fn debug_assert_not_misrouted(&self) {
        if let Self::Custom(name) = self {
            debug_assert!(
                matches!(Self::from_single_name(name.as_str()), Self::Custom(_)),
                "TraitKind::Custom({name:?}) wraps a well-known trait name — \
                 use `TraitKind::from_path` or the typed variant directly \
                 (e.g. `TraitKind::{name}`)."
            );
        }
    }

    pub fn from_path(segments: &[String]) -> Self {
        if let Some(math) = MathTrait::from_path(segments) {
            return Self::Math(math);
        }
        // Normalize Rng aliases
        let last = segments.last().cloned().unwrap_or_default();
        // Single-segment: check for well-known trait names
        if segments.len() == 1 {
            return Self::from_single_name(&last);
        }
        // RngCore / CryptoRng in qualified paths
        if matches!(last.as_str(), "RngCore" | "CryptoRng") {
            return Self::Rng;
        }
        Self::External {
            path: segments.to_vec(),
        }
    }

    /// Classify a bare (single-segment) trait name.
    pub fn from_single_name(name: &str) -> Self {
        match name {
            "ArrayLength" => Self::ArrayLength,
            "ArraySize" => Self::ArraySize,
            "VoleArray" => Self::VoleArray,
            "LengthDoubler" => Self::LengthDoubler,
            "BlockEncrypt" => Self::BlockEncrypt,
            "BlockCipher" => Self::BlockCipher,
            "Digest" => Self::Digest,
            "Rng" | "RngCore" | "CryptoRng" => Self::Rng,
            _ => Self::Custom(name.to_string()),
        }
    }
}

impl fmt::Display for TraitKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Guardrail fires on every format call for Custom variants.
        self.debug_assert_not_misrouted();
        match self {
            Self::Math(m) => write!(f, "{:?}", m),
            Self::External { path } => write!(f, "{}", path.join("::")),
            Self::Custom(name) => write!(f, "{}", name),
            Self::Into(ty) => write!(f, "Into<{}>", ty),
            Self::AsRef(ty) => write!(f, "AsRef<{}>", ty),
            Self::Fn(input, ty) => match input {
                FnInput::BytesSlice => write!(f, "FnMut(&[u8]) -> {}", ty),
                FnInput::Size => write!(f, "FnMut(usize) -> {}", ty),
                FnInput::Bool => write!(f, "FnMut(bool) -> {}", ty),
            },
            Self::ArrayLength => write!(f, "ArrayLength"),
            Self::ArraySize => write!(f, "ArraySize"),
            Self::VoleArray => write!(f, "VoleArray"),
            Self::LengthDoubler => write!(f, "LengthDoubler"),
            Self::BlockEncrypt => write!(f, "BlockEncrypt"),
            Self::BlockCipher => write!(f, "BlockCipher"),
            Self::Digest => write!(f, "Digest"),
            Self::Rng => write!(f, "Rng"),
        }
    }
}

/// Input kinds allowed for generated Fn-like traits
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum FnInput {
    BytesSlice,
    Size,
    Bool,
}

/// VOLE-specific method names
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
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

/// Well-known method names that the compiler has specific handling for.
///
/// Every variant here corresponds to a Rust method name that at least one
/// compiler pass matches against.  Adding a new variant here removes one
/// `if name == "..."` / `match s.as_str()` pattern from processing code.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum StdMethod {
    // ---- Standard Rust methods (former Std whitelist) ----
    Clone,
    Default,
    Into,
    From,
    AsRef,
    AsSlice,
    AsPtr,
    Get,
    Len,
    IsEmpty,
    Contains,
    Unwrap,
    UnwrapOr,
    UnwrapOrDefault,
    Expect,
    ToUsize,
    ToString,
    WrappingAdd,
    WrappingSub,
    CheckedAdd,
    CheckedSub,
    SaturatingAdd,
    SaturatingSub,
    Bitxor,
    Deref,
    // ---- Previously Unknown but switch-cased in processing code ----
    EncryptBlock,
    Finalize,
    ToVec,
    Ilog2,
    Shl,
    Shr,
    MapOr,
    Update,
    Cloned,
    At,
    // ---- Integer bit/byte operations (need TS-specific translations) ----
    WrappingShl,
    WrappingShr,
    SaturatingMul,
    CountOnes,
    LeadingZeros,
    TrailingZeros,
    FromLeBytes,
    ToLeBytes,
    FromBeBytes,
    ToBeBytes,
    Pow,
    Abs,
    Min,
    Max,
    CopyFromSlice,
    Rev,
    Sort,
    Fill,
    Truncate,
    Push,
    Pop,
    WithCapacity,
    New,
    WrappingMul,
    WrappingNeg,
    OverflowingAdd,
    OverflowingSub,
}

impl StdMethod {
    pub fn try_from_str(s: &str) -> Option<Self> {
        match s {
            "clone" => Some(Self::Clone),
            "default" => Some(Self::Default),
            "into" => Some(Self::Into),
            "from" => Some(Self::From),
            "as_ref" => Some(Self::AsRef),
            "as_slice" => Some(Self::AsSlice),
            "as_ptr" => Some(Self::AsPtr),
            "get" => Some(Self::Get),
            "len" => Some(Self::Len),
            "is_empty" => Some(Self::IsEmpty),
            "contains" => Some(Self::Contains),
            "unwrap" => Some(Self::Unwrap),
            "unwrap_or" => Some(Self::UnwrapOr),
            "unwrap_or_default" => Some(Self::UnwrapOrDefault),
            "expect" => Some(Self::Expect),
            "to_usize" => Some(Self::ToUsize),
            "to_string" => Some(Self::ToString),
            "wrapping_add" => Some(Self::WrappingAdd),
            "wrapping_sub" => Some(Self::WrappingSub),
            "checked_add" => Some(Self::CheckedAdd),
            "checked_sub" => Some(Self::CheckedSub),
            "saturating_add" => Some(Self::SaturatingAdd),
            "saturating_sub" => Some(Self::SaturatingSub),
            "bitxor" => Some(Self::Bitxor),
            "deref" => Some(Self::Deref),
            "encrypt_block" => Some(Self::EncryptBlock),
            "finalize" => Some(Self::Finalize),
            "to_vec" => Some(Self::ToVec),
            "ilog2" => Some(Self::Ilog2),
            "shl" => Some(Self::Shl),
            "shr" => Some(Self::Shr),
            "map_or" => Some(Self::MapOr),
            "update" => Some(Self::Update),
            "cloned" => Some(Self::Cloned),
            "at" => Some(Self::At),
            "wrapping_shl" => Some(Self::WrappingShl),
            "wrapping_shr" => Some(Self::WrappingShr),
            "saturating_mul" => Some(Self::SaturatingMul),
            "count_ones" => Some(Self::CountOnes),
            "leading_zeros" => Some(Self::LeadingZeros),
            "trailing_zeros" => Some(Self::TrailingZeros),
            "from_le_bytes" => Some(Self::FromLeBytes),
            "to_le_bytes" => Some(Self::ToLeBytes),
            "from_be_bytes" => Some(Self::FromBeBytes),
            "to_be_bytes" => Some(Self::ToBeBytes),
            "pow" => Some(Self::Pow),
            "abs" => Some(Self::Abs),
            "min" => Some(Self::Min),
            "max" => Some(Self::Max),
            "copy_from_slice" => Some(Self::CopyFromSlice),
            "rev" => Some(Self::Rev),
            "sort" => Some(Self::Sort),
            "fill" => Some(Self::Fill),
            "truncate" => Some(Self::Truncate),
            "push" => Some(Self::Push),
            "pop" => Some(Self::Pop),
            "with_capacity" => Some(Self::WithCapacity),
            "new" => Some(Self::New),
            "wrapping_mul" => Some(Self::WrappingMul),
            "wrapping_neg" => Some(Self::WrappingNeg),
            "overflowing_add" => Some(Self::OverflowingAdd),
            "overflowing_sub" => Some(Self::OverflowingSub),
            _ => None,
        }
    }

    /// Returns the Rust source spelling of this method name.
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Clone => "clone",
            Self::Default => "default",
            Self::Into => "into",
            Self::From => "from",
            Self::AsRef => "as_ref",
            Self::AsSlice => "as_slice",
            Self::AsPtr => "as_ptr",
            Self::Get => "get",
            Self::Len => "len",
            Self::IsEmpty => "is_empty",
            Self::Contains => "contains",
            Self::Unwrap => "unwrap",
            Self::UnwrapOr => "unwrap_or",
            Self::UnwrapOrDefault => "unwrap_or_default",
            Self::Expect => "expect",
            Self::ToUsize => "to_usize",
            Self::ToString => "to_string",
            Self::WrappingAdd => "wrapping_add",
            Self::WrappingSub => "wrapping_sub",
            Self::CheckedAdd => "checked_add",
            Self::CheckedSub => "checked_sub",
            Self::SaturatingAdd => "saturating_add",
            Self::SaturatingSub => "saturating_sub",
            Self::Bitxor => "bitxor",
            Self::Deref => "deref",
            Self::EncryptBlock => "encrypt_block",
            Self::Finalize => "finalize",
            Self::ToVec => "to_vec",
            Self::Ilog2 => "ilog2",
            Self::Shl => "shl",
            Self::Shr => "shr",
            Self::MapOr => "map_or",
            Self::Update => "update",
            Self::Cloned => "cloned",
            Self::At => "at",
            Self::WrappingShl => "wrapping_shl",
            Self::WrappingShr => "wrapping_shr",
            Self::SaturatingMul => "saturating_mul",
            Self::CountOnes => "count_ones",
            Self::LeadingZeros => "leading_zeros",
            Self::TrailingZeros => "trailing_zeros",
            Self::FromLeBytes => "from_le_bytes",
            Self::ToLeBytes => "to_le_bytes",
            Self::FromBeBytes => "from_be_bytes",
            Self::ToBeBytes => "to_be_bytes",
            Self::Pow => "pow",
            Self::Abs => "abs",
            Self::Min => "min",
            Self::Max => "max",
            Self::CopyFromSlice => "copy_from_slice",
            Self::Rev => "rev",
            Self::Sort => "sort",
            Self::Fill => "fill",
            Self::Truncate => "truncate",
            Self::Push => "push",
            Self::Pop => "pop",
            Self::WithCapacity => "with_capacity",
            Self::New => "new",
            Self::WrappingMul => "wrapping_mul",
            Self::WrappingNeg => "wrapping_neg",
            Self::OverflowingAdd => "overflowing_add",
            Self::OverflowingSub => "overflowing_sub",
        }
    }
}

impl fmt::Display for StdMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Unified method classification.
///
/// Use `Known(StdMethod::*)` for any method the compiler has specific handling
/// for, and `Other(String)` for domain-specific or user-defined methods (e.g.
/// garbled-circuit helpers like `and_via_table`).
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum MethodKind {
    Vole(VoleMethod),
    /// A method the compiler knows about and has explicit handling for.
    Known(StdMethod),
    /// A method with no special compiler handling; emitted as-is.
    Other(String),
}

impl MethodKind {
    pub fn from_str(s: &str) -> Self {
        if let Some(v) = VoleMethod::try_from_str(s) {
            return Self::Vole(v);
        }
        if let Some(m) = StdMethod::try_from_str(s) {
            return Self::Known(m);
        }
        Self::Other(s.to_string())
    }

    /// Asserts in debug builds that this `Other` variant does not wrap a name
    /// that belongs in [`MethodKind::Known`], and that the name is a valid
    /// identifier.
    ///
    /// Call this at every `MethodKind::Other` code-generation site (Rust
    /// printer, TypeScript printer, LIR codegen) so that misrouted construction
    /// is caught immediately during testing rather than silently producing
    /// wrong output.
    ///
    /// ```
    /// # use volar_compiler::ir::{MethodKind, StdMethod};
    /// // Correct — "and_via_table" is genuinely domain-specific
    /// MethodKind::Other("and_via_table".into()).debug_assert_not_misrouted();
    /// ```
    #[inline]
    pub fn debug_assert_not_misrouted(&self) {
        if let Self::Other(s) = self {
            debug_assert!(
                StdMethod::try_from_str(s.as_str()).is_none(),
                "MethodKind::Other({s:?}) wraps a well-known method name — \
                 use `MethodKind::from_str(\"{s}\")` or \
                 `MethodKind::Known(StdMethod::…)` to construct it correctly. \
                 The TypeScript and other backends have specialised emit logic \
                 for this method that would be bypassed with Other."
            );
            debug_assert!(
                s.chars().all(|c| c.is_ascii_alphanumeric() || c == '_'),
                "MethodKind::Other({s:?}) is not a valid identifier"
            );
        }
    }
}

/// Associated type names in traits
#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
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

/// Fully-qualified path of an IR item: `[crate_name, module_stem, …, item_name]`.
/// The last element is always the bare identifier. An empty vec means
/// the item is synthetic or built-in (no origin).
pub type IrPath = Vec<String>;

/// Return the bare name (last segment) of an `IrPath`.
pub fn ir_path_name(path: &IrPath) -> &str {
    path.last().map(|s| s.as_str()).unwrap_or("")
}

/// Build the full `IrPath` for an item given its `module_path` and bare `name`.
pub fn item_irpath(module_path: &[String], name: &str) -> IrPath {
    let mut v = module_path.to_vec();
    v.push(name.to_string());
    v
}

/// A `pub const` declaration captured from the spec source.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrConst {
    pub name: String,
    /// Origin module path (without the item name). Empty = synthetic/built-in.
    pub module_path: Vec<String>,
    pub ty: IrType,
    pub value: IrExpr,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrModule<F, P: Clone + Default = ()> {
    pub name: String,
    pub structs: Vec<IrStruct>,
    pub enums: Vec<IrEnum>,
    pub traits: Vec<IrTrait>,
    pub impls: Vec<IrImpl<P>>,
    pub functions: Vec<F>,
    pub type_aliases: Vec<IrTypeAlias>,
    pub consts: Vec<IrConst>,
}

impl<F, P: Clone + Default> Default for IrModule<F, P> {
    fn default() -> Self {
        Self {
            name: Default::default(),
            structs: Default::default(),
            enums: Default::default(),
            traits: Default::default(),
            impls: Default::default(),
            functions: Default::default(),
            type_aliases: Default::default(),
            consts: Default::default(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrStruct {
    pub kind: StructKind,
    /// Origin module path (without the item name). Empty = synthetic/built-in.
    pub module_path: Vec<String>,
    pub generics: Vec<IrGenericParam>,
    pub fields: Vec<IrField>,
    pub is_tuple: bool,
    /// If the struct carries a `/// @volar-native: <Type>` doc comment, this
    /// field records the target Volar IR primitive type.  The LIR lowering
    /// pipeline maps such structs to [`LirType::Native`] and `VolarIrTarget`
    /// emits a single field-element `IRVarId` rather than decomposing the
    /// struct into individual bits.
    pub native_volar_type: Option<volar_ir_common::Type>,
    /// Derive trait names captured from `#[derive(...)]` attributes.
    ///
    /// When non-empty, the printer emits exactly these derives.  When empty
    /// (programmatically constructed structs), the printer falls back to a
    /// default set (`Debug, Default`).
    pub derives: Vec<String>,
}

/// A tagged union (enum) declaration.
///
/// Each variant can be unit (no data), tuple-like (positional fields), or
/// struct-like (named fields).  The representation is backend-agnostic:
///
/// - **Rust**: emits a standard `enum` with `#[derive(Debug)]`.
/// - **TypeScript** (future): emits discriminated-union types
///   `{ tag: "Variant1", ... } | { tag: "Variant2", ... }`.
/// - **C** (future): emits a tagged struct with a discriminant enum and a union.
///
/// Variant construction and pattern matching use existing IR nodes:
/// - Unit variant: `IrExpr::Path { segments: ["Enum", "Variant"] }`
/// - Tuple variant: `IrExpr::Call` with path `Enum::Variant`
/// - Struct variant: `IrExpr::StructExpr { kind: Custom("Enum::Variant"), .. }`
/// - Patterns: `IrPattern::TupleStruct` / `IrPattern::Struct` with
///   `StructKind::Custom("Enum::Variant")`
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrEnum {
    pub kind: StructKind,
    pub generics: Vec<IrGenericParam>,
    pub variants: Vec<IrEnumVariant>,
    /// Derive trait names captured from `#[derive(...)]` attributes.
    ///
    /// When non-empty, the printer emits exactly these derives.  When empty
    /// (programmatically constructed enums), the printer falls back to a
    /// default set (`Debug`).
    pub derives: Vec<String>,
}

/// A single variant of an [`IrEnum`].
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrEnumVariant {
    pub name: String,
    pub fields: IrEnumVariantData,
}

/// The data carried by an enum variant.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum IrEnumVariantData {
    /// Unit variant: `Variant` (no payload).
    Unit,
    /// Tuple variant: `Variant(T1, T2, ...)`.
    Tuple(Vec<IrType>),
    /// Struct variant: `Variant { field1: T1, field2: T2, ... }`.
    Struct(Vec<IrField>),
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrTrait {
    pub kind: TraitKind,
    /// Origin module path (without the trait name). Empty = synthetic/built-in.
    pub module_path: Vec<String>,
    pub generics: Vec<IrGenericParam>,
    pub super_traits: Vec<IrTraitBound>,
    pub items: Vec<IrTraitItem>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum IrTraitItem {
    Method(IrMethodSig),
    AssociatedType {
        name: AssociatedType,
        bounds: Vec<IrTraitBound>,
        default: Option<IrType>,
    },
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrImpl<P: Clone + Default = ()> {
    pub generics: Vec<IrGenericParam>,
    pub trait_: Option<IrTraitRef>,
    pub self_ty: IrType,
    pub where_clause: Vec<IrWherePredicate>,
    pub items: Vec<IrImplItem<P>>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrTraitRef {
    pub kind: TraitKind,
    pub type_args: Vec<IrType>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum IrImplItem<P: Clone + Default = ()> {
    Method(IrFunction<P>),
    AssociatedType { name: AssociatedType, ty: IrType },
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrMethodSig {
    pub name: String,
    pub generics: Vec<IrGenericParam>,
    pub receiver: Option<IrReceiver>,
    pub params: Vec<IrParam>,
    pub return_type: Option<IrType>,
    pub where_clause: Vec<IrWherePredicate>,
}

/// Indicates whether a function is a normal body, an oracle, an action, or an RNG.
///
/// Recognised by the parser from `#[oracle]`, `#[action]`, and `#[rng]` attributes
/// on function items.  External proc-macros may attach these attributes to library
/// functions without modifying Volar source.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum ExternalKind {
    /// A regular function with a body.
    Normal,
    /// A named pure oracle (deterministic, no side effects).
    Oracle,
    /// A named conditional action (impure, guard-conditional).
    Action,
    /// A random-number generator stub.
    Rng,
    /// A type-only stub: carries signature/return-type info for LIR codegen
    /// but is not emitted by Rust/TS printers (the real implementation is
    /// imported via `use`).
    TypeStub,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrFunction<P: Clone + Default = ()> {
    pub name: String,
    /// Origin module path (without the item name). Empty = synthetic/built-in.
    pub module_path: Vec<String>,
    pub generics: Vec<IrGenericParam>,
    pub receiver: Option<IrReceiver>,
    pub params: Vec<IrParam>,
    pub return_type: Option<IrType>,
    pub where_clause: Vec<IrWherePredicate>,
    pub body: IrBlock<P>,
    /// What kind of external primitive this function represents.
    /// Default: `ExternalKind::Normal`.
    pub external_kind: ExternalKind,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrParam {
    pub name: String,
    pub ty: IrType,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrField {
    pub name: String,
    pub ty: IrType,
    pub public: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum IrGenericParamKind {
    Type,
    Const,
    Lifetime,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrGenericParam {
    pub name: String,
    pub kind: IrGenericParamKind,
    /// For `Const` params: the type of the constant (e.g. `usize`).
    /// `None` defaults to `usize` in the printer.
    pub const_ty: Option<IrType>,
    pub bounds: Vec<IrTraitBound>,
    pub default: Option<IrType>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum IrWherePredicate {
    TypeBound {
        ty: IrType,
        bounds: Vec<IrTraitBound>,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum IrReceiver {
    Value,
    Ref,
    RefMut,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrTypeAlias {
    pub name: String,
    /// Origin module path (without the item name). Empty = synthetic/built-in.
    pub module_path: Vec<String>,
    pub generics: Vec<IrGenericParam>,
    pub target: IrType,
}

#[derive(Debug, Clone, PartialEq, Default)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrBlock<P: Clone + Default = ()> {
    pub stmts: Vec<IrStmt<P>>,
    pub stmt_provs: Vec<P>,
    pub expr: Option<Box<IrExpr<P>>>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[non_exhaustive]
pub enum IrStmt<P: Clone + Default = ()> {
    Let {
        pattern: IrPattern,
        ty: Option<IrType>,
        init: Option<IrExpr<P>>,
    },
    Semi(IrExpr<P>),
    Expr(IrExpr<P>),
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrMatchArm<P: Clone + Default = ()> {
    pub pattern: IrPattern,
    pub guard: Option<IrExpr<P>>,
    pub body: IrExpr<P>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrClosureParam {
    pub pattern: IrPattern,
    pub ty: Option<IrType>,
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[non_exhaustive]
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
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrTraitBound {
    pub trait_kind: TraitKind,
    pub type_args: Vec<IrType>,
    pub assoc_bindings: Vec<(AssociatedType, IrType)>,
}

impl fmt::Display for IrTraitBound {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Guardrail: catches Custom("Digest") etc. at any Display call site.
        self.trait_kind.debug_assert_not_misrouted();
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
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[non_exhaustive]
pub enum IrExpr<P: Clone + Default = ()> {
    Lit(IrLit),
    Var(String),
    Path {
        segments: Vec<String>,
        type_args: Vec<IrType>,
    },
    Binary {
        op: SpecBinOp,
        left: Box<IrExpr<P>>,
        right: Box<IrExpr<P>>,
    },
    Unary {
        op: SpecUnaryOp,
        expr: Box<IrExpr<P>>,
    },
    MethodCall {
        receiver: Box<IrExpr<P>>,
        method: MethodKind,
        type_args: Vec<IrType>,
        args: Vec<IrExpr<P>>,
    },
    Call {
        func: Box<IrExpr<P>>,
        args: Vec<IrExpr<P>>,
    },
    Field {
        base: Box<IrExpr<P>>,
        field: String,
    },
    Index {
        base: Box<IrExpr<P>>,
        index: Box<IrExpr<P>>,
    },
    StructExpr {
        kind: StructKind,
        type_args: Vec<IrType>,
        fields: Vec<(String, IrExpr<P>)>,
        rest: Option<Box<IrExpr<P>>>,
    },
    Tuple(Vec<IrExpr<P>>),
    Array(Vec<IrExpr<P>>),
    /// A fixed-size array literal `[a, b, c]` (as opposed to `Array`, which prints as `vec![...]`).
    FixedArray(Vec<IrExpr<P>>),
    Repeat {
        elem: Box<IrExpr<P>>,
        len: Box<IrExpr<P>>,
    },
    ArrayGenerate {
        elem_ty: Option<Box<IrType>>,
        len: ArrayLength,
        index_var: String,
        body: Box<IrExpr<P>>,
    },
    /// Calls `T::default()` for a given type.
    ///
    /// When `ty` is an array/vector type (e.g. `Array { elem: u8, len: N }`),
    /// dyn lowering expands this recursively:
    ///   `DefaultValue { ty: Array<T, N> }`
    ///   → `IterPipeline(0..N).map(_ => DefaultValue { ty: T })`
    ///
    /// The Rust printer emits `<T>::default()` or `vec![<T>::default(); N]`.
    /// The TS printer handles leaf defaults (primitives, type params).
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
    IterPipeline(IrIterChain<P>),

    /// Non-iterator element-wise map: `receiver.map(|var| body)`
    /// Used for GenericArray::map and [T; N]::map.
    /// Length-preserving; bounded by the receiver's length.
    RawMap {
        receiver: Box<IrExpr<P>>,
        elem_var: IrPattern,
        body: Box<IrExpr<P>>,
    },

    /// Non-iterator element-wise zip-with-map: `receiver.zip(other, |a, b| body)`
    /// Used for GenericArray::zip. Length-preserving; bounded.
    RawZip {
        left: Box<IrExpr<P>>,
        right: Box<IrExpr<P>>,
        left_var: IrPattern,
        right_var: IrPattern,
        body: Box<IrExpr<P>>,
    },

    /// Non-iterator fold over array: `receiver.fold(init, |acc, elem| body)`
    /// When applied directly on GenericArray/[T;N] (no .iter() prefix).
    RawFold {
        receiver: Box<IrExpr<P>>,
        init: Box<IrExpr<P>>,
        acc_var: IrPattern,
        elem_var: IrPattern,
        body: Box<IrExpr<P>>,
    },

    BoundedLoop {
        var: String,
        start: Box<IrExpr<P>>,
        end: Box<IrExpr<P>>,
        inclusive: bool,
        body: IrBlock<P>,
    },
    IterLoop {
        pattern: IrPattern,
        collection: Box<IrExpr<P>>,
        body: IrBlock<P>,
    },
    Block(IrBlock<P>),
    If {
        cond: Box<IrExpr<P>>,
        then_branch: IrBlock<P>,
        else_branch: Option<Box<IrExpr<P>>>,
    },
    Match {
        expr: Box<IrExpr<P>>,
        arms: Vec<IrMatchArm<P>>,
    },
    Closure {
        params: Vec<IrClosureParam>,
        ret_type: Option<Box<IrType>>,
        body: Box<IrExpr<P>>,
    },
    Cast {
        expr: Box<IrExpr<P>>,
        ty: Box<IrType>,
    },
    Return(Option<Box<IrExpr<P>>>),
    Break(Option<Box<IrExpr<P>>>),
    Continue,
    Assign {
        left: Box<IrExpr<P>>,
        right: Box<IrExpr<P>>,
    },
    AssignOp {
        op: SpecBinOp,
        left: Box<IrExpr<P>>,
        right: Box<IrExpr<P>>,
    },
    Range {
        start: Option<Box<IrExpr<P>>>,
        end: Option<Box<IrExpr<P>>>,
        inclusive: bool,
    },
    TypenumUsize {
        ty: Box<IrType>,
    },
    WhileLoop {
        cond: Box<IrExpr<P>>,
        body: IrBlock<P>,
    },
    Unreachable,
    Try(Box<IrExpr<P>>),
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[non_exhaustive]
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

    /// Return a copy with the outermost binding marked `mut`.
    pub fn as_mut(self) -> Self {
        match self {
            IrPattern::Ident { name, subpat, .. } => IrPattern::Ident {
                mutable: true,
                name,
                subpat,
            },
            other => other, // can't meaningfully add mut to Wild/Tuple/etc.
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
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
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
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
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
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


        module_path: vec![],        generics: vec![IrGenericParam {
            name: "Rhs".into(),
            kind: IrGenericParamKind::Type,
            const_ty: None,
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


        module_path: vec![],        generics: vec![],
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


            module_path: vec![],            generics: vec![IrGenericParam {
                name: "Rhs".into(),
                kind: IrGenericParamKind::Type,
                const_ty: None,
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


            module_path: vec![],            generics: vec![],
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


            module_path: vec![],            generics: vec![IrGenericParam {
                name: "Rhs".into(),
                kind: IrGenericParamKind::Type,
                const_ty: None,
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


            module_path: vec![],            generics: vec![],
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


            module_path: vec![],            generics: vec![],
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


            module_path: vec![],            generics: vec![],
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


            module_path: vec![],            generics: vec![],
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


            module_path: vec![],            generics: vec![],
            super_traits: vec![],
            items: vec![],
        },
        // --- Crypto traits ---
        // BlockEncrypt
        IrTrait {
            kind: TraitKind::BlockEncrypt,


            module_path: vec![],            generics: vec![],
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
            kind: TraitKind::BlockCipher,


            module_path: vec![],            generics: vec![],
            super_traits: vec![],
            items: vec![IrTraitItem::AssociatedType {
                name: AssociatedType::BlockSize,
                bounds: vec![],
                default: None,
            }],
        },
        // Digest
        IrTrait {
            kind: TraitKind::Digest,


            module_path: vec![],            generics: vec![],
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
            kind: TraitKind::ArrayLength,


            module_path: vec![],            generics: vec![IrGenericParam {
                name: "T".into(),
                kind: IrGenericParamKind::Type,
                const_ty: None,
                bounds: vec![],
                default: None,
            }],
            super_traits: vec![],
            items: vec![],
        },
        // Rng — minimal definition
        IrTrait {
            kind: TraitKind::Rng,


            module_path: vec![],            generics: vec![],
            super_traits: vec![],
            items: vec![],
        },
    ]
}

// ============================================================================
// Provenance mapping
// ============================================================================

/// Uniform interface for transforming provenance annotations from `P` to `Q`.
///
/// All parameterized IR types implement this trait, enabling generic
/// provenance-mapping in [`IrModule::map_prov`].
pub trait MapProv<P: Clone + Default, Q: Clone + Default>: Sized {
    type Output;
    fn map_prov(self, f: &impl Fn(P) -> Q) -> Self::Output;
}

impl<P: Clone + Default, Q: Clone + Default> MapProv<P, Q> for IrExpr<P> {
    type Output = IrExpr<Q>;
    fn map_prov(self, f: &impl Fn(P) -> Q) -> IrExpr<Q> {
        match self {
            IrExpr::Lit(l) => IrExpr::Lit(l),
            IrExpr::Var(v) => IrExpr::Var(v),
            IrExpr::Path { segments, type_args } => IrExpr::Path { segments, type_args },
            IrExpr::Binary { op, left, right } =>
                IrExpr::Binary { op, left: Box::new(left.map_prov(f)), right: Box::new(right.map_prov(f)) },
            IrExpr::Unary { op, expr } =>
                IrExpr::Unary { op, expr: Box::new(expr.map_prov(f)) },
            IrExpr::MethodCall { receiver, method, type_args, args } =>
                IrExpr::MethodCall { receiver: Box::new(receiver.map_prov(f)), method, type_args, args: args.into_iter().map(|a| a.map_prov(f)).collect() },
            IrExpr::Call { func, args } =>
                IrExpr::Call { func: Box::new(func.map_prov(f)), args: args.into_iter().map(|a| a.map_prov(f)).collect() },
            IrExpr::Field { base, field } =>
                IrExpr::Field { base: Box::new(base.map_prov(f)), field },
            IrExpr::Index { base, index } =>
                IrExpr::Index { base: Box::new(base.map_prov(f)), index: Box::new(index.map_prov(f)) },
            IrExpr::StructExpr { kind, type_args, fields, rest } =>
                IrExpr::StructExpr { kind, type_args, fields: fields.into_iter().map(|(n, e)| (n, e.map_prov(f))).collect(), rest: rest.map(|r| Box::new(r.map_prov(f))) },
            IrExpr::Tuple(es) => IrExpr::Tuple(es.into_iter().map(|e| e.map_prov(f)).collect()),
            IrExpr::Array(es) => IrExpr::Array(es.into_iter().map(|e| e.map_prov(f)).collect()),
            IrExpr::FixedArray(es) => IrExpr::FixedArray(es.into_iter().map(|e| e.map_prov(f)).collect()),
            IrExpr::Repeat { elem, len } =>
                IrExpr::Repeat { elem: Box::new(elem.map_prov(f)), len: Box::new(len.map_prov(f)) },
            IrExpr::ArrayGenerate { elem_ty, len, index_var, body } =>
                IrExpr::ArrayGenerate { elem_ty, len, index_var, body: Box::new(body.map_prov(f)) },
            IrExpr::DefaultValue { ty } => IrExpr::DefaultValue { ty },
            IrExpr::LengthOf(l) => IrExpr::LengthOf(l),
            IrExpr::IterPipeline(chain) => IrExpr::IterPipeline(chain.map_prov(f)),
            IrExpr::RawMap { receiver, elem_var, body } =>
                IrExpr::RawMap { receiver: Box::new(receiver.map_prov(f)), elem_var, body: Box::new(body.map_prov(f)) },
            IrExpr::RawZip { left, right, left_var, right_var, body } =>
                IrExpr::RawZip { left: Box::new(left.map_prov(f)), right: Box::new(right.map_prov(f)), left_var, right_var, body: Box::new(body.map_prov(f)) },
            IrExpr::RawFold { receiver, init, acc_var, elem_var, body } =>
                IrExpr::RawFold { receiver: Box::new(receiver.map_prov(f)), init: Box::new(init.map_prov(f)), acc_var, elem_var, body: Box::new(body.map_prov(f)) },
            IrExpr::BoundedLoop { var, start, end, inclusive, body } =>
                IrExpr::BoundedLoop { var, start: Box::new(start.map_prov(f)), end: Box::new(end.map_prov(f)), inclusive, body: body.map_prov(f) },
            IrExpr::IterLoop { pattern, collection, body } =>
                IrExpr::IterLoop { pattern, collection: Box::new(collection.map_prov(f)), body: body.map_prov(f) },
            IrExpr::Block(b) => IrExpr::Block(b.map_prov(f)),
            IrExpr::If { cond, then_branch, else_branch } =>
                IrExpr::If { cond: Box::new(cond.map_prov(f)), then_branch: then_branch.map_prov(f), else_branch: else_branch.map(|e| Box::new(e.map_prov(f))) },
            IrExpr::Match { expr, arms } =>
                IrExpr::Match { expr: Box::new(expr.map_prov(f)), arms: arms.into_iter().map(|a| a.map_prov(f)).collect() },
            IrExpr::Closure { params, ret_type, body } =>
                IrExpr::Closure { params, ret_type, body: Box::new(body.map_prov(f)) },
            IrExpr::Cast { expr, ty } => IrExpr::Cast { expr: Box::new(expr.map_prov(f)), ty },
            IrExpr::Return(e) => IrExpr::Return(e.map(|e| Box::new(e.map_prov(f)))),
            IrExpr::Break(e) => IrExpr::Break(e.map(|e| Box::new(e.map_prov(f)))),
            IrExpr::Continue => IrExpr::Continue,
            IrExpr::Assign { left, right } =>
                IrExpr::Assign { left: Box::new(left.map_prov(f)), right: Box::new(right.map_prov(f)) },
            IrExpr::AssignOp { op, left, right } =>
                IrExpr::AssignOp { op, left: Box::new(left.map_prov(f)), right: Box::new(right.map_prov(f)) },
            IrExpr::Range { start, end, inclusive } =>
                IrExpr::Range { start: start.map(|e| Box::new(e.map_prov(f))), end: end.map(|e| Box::new(e.map_prov(f))), inclusive },
            IrExpr::TypenumUsize { ty } => IrExpr::TypenumUsize { ty },
            IrExpr::WhileLoop { cond, body } =>
                IrExpr::WhileLoop { cond: Box::new(cond.map_prov(f)), body: body.map_prov(f) },
            IrExpr::Unreachable => IrExpr::Unreachable,
            IrExpr::Try(e) => IrExpr::Try(Box::new(e.map_prov(f))),
        }
    }
}

impl<P: Clone + Default, Q: Clone + Default> MapProv<P, Q> for IrIterChain<P> {
    type Output = IrIterChain<Q>;
    fn map_prov(self, f: &impl Fn(P) -> Q) -> IrIterChain<Q> {
        IrIterChain {
            source: self.source.map_prov(f),
            steps: self.steps.into_iter().map(|s| s.map_prov(f)).collect(),
            terminal: self.terminal.map_prov(f),
        }
    }
}

impl<P: Clone + Default, Q: Clone + Default> MapProv<P, Q> for IterChainSource<P> {
    type Output = IterChainSource<Q>;
    fn map_prov(self, f: &impl Fn(P) -> Q) -> IterChainSource<Q> {
        match self {
            IterChainSource::Method { collection, method } =>
                IterChainSource::Method { collection: Box::new(collection.map_prov(f)), method },
            IterChainSource::Range { start, end, inclusive } =>
                IterChainSource::Range { start: Box::new(start.map_prov(f)), end: Box::new(end.map_prov(f)), inclusive },
            IterChainSource::Zip { left, right } =>
                IterChainSource::Zip { left: Box::new(left.map_prov(f)), right: Box::new(right.map_prov(f)) },
        }
    }
}

impl<P: Clone + Default, Q: Clone + Default> MapProv<P, Q> for IterStep<P> {
    type Output = IterStep<Q>;
    fn map_prov(self, f: &impl Fn(P) -> Q) -> IterStep<Q> {
        match self {
            IterStep::Map { var, body } => IterStep::Map { var, body: Box::new(body.map_prov(f)) },
            IterStep::Filter { var, body } => IterStep::Filter { var, body: Box::new(body.map_prov(f)) },
            IterStep::FilterMap { var, body } => IterStep::FilterMap { var, body: Box::new(body.map_prov(f)) },
            IterStep::FlatMap { var, body } => IterStep::FlatMap { var, body: Box::new(body.map_prov(f)) },
            IterStep::Enumerate => IterStep::Enumerate,
            IterStep::Take { count } => IterStep::Take { count: Box::new(count.map_prov(f)) },
            IterStep::Skip { count } => IterStep::Skip { count: Box::new(count.map_prov(f)) },
            IterStep::Chain { other } => IterStep::Chain { other: Box::new(other.map_prov(f)) },
        }
    }
}

impl<P: Clone + Default, Q: Clone + Default> MapProv<P, Q> for IterTerminal<P> {
    type Output = IterTerminal<Q>;
    fn map_prov(self, f: &impl Fn(P) -> Q) -> IterTerminal<Q> {
        match self {
            IterTerminal::Collect => IterTerminal::Collect,
            IterTerminal::CollectTyped(ty) => IterTerminal::CollectTyped(ty),
            IterTerminal::Fold { init, acc_var, elem_var, body } =>
                IterTerminal::Fold { init: Box::new(init.map_prov(f)), acc_var, elem_var, body: Box::new(body.map_prov(f)) },
            IterTerminal::Lazy => IterTerminal::Lazy,
        }
    }
}

impl<P: Clone + Default, Q: Clone + Default> MapProv<P, Q> for IrMatchArm<P> {
    type Output = IrMatchArm<Q>;
    fn map_prov(self, f: &impl Fn(P) -> Q) -> IrMatchArm<Q> {
        IrMatchArm {
            pattern: self.pattern,
            guard: self.guard.map(|g| g.map_prov(f)),
            body: self.body.map_prov(f),
        }
    }
}

impl<P: Clone + Default, Q: Clone + Default> MapProv<P, Q> for IrStmt<P> {
    type Output = IrStmt<Q>;
    fn map_prov(self, f: &impl Fn(P) -> Q) -> IrStmt<Q> {
        match self {
            IrStmt::Let { pattern, ty, init } =>
                IrStmt::Let { pattern, ty, init: init.map(|e| e.map_prov(f)) },
            IrStmt::Semi(e) => IrStmt::Semi(e.map_prov(f)),
            IrStmt::Expr(e) => IrStmt::Expr(e.map_prov(f)),
        }
    }
}

impl<P: Clone + Default, Q: Clone + Default> MapProv<P, Q> for IrBlock<P> {
    type Output = IrBlock<Q>;
    fn map_prov(self, f: &impl Fn(P) -> Q) -> IrBlock<Q> {
        IrBlock {
            stmts: self.stmts.into_iter().map(|s| s.map_prov(f)).collect(),
            stmt_provs: self.stmt_provs.into_iter().map(|p| f(p)).collect(),
            expr: self.expr.map(|e| Box::new(e.map_prov(f))),
        }
    }
}

impl<P: Clone + Default, Q: Clone + Default> MapProv<P, Q> for IrFunction<P> {
    type Output = IrFunction<Q>;
    fn map_prov(self, f: &impl Fn(P) -> Q) -> IrFunction<Q> {
        IrFunction {
            name: self.name,
            module_path: self.module_path,
            generics: self.generics,
            receiver: self.receiver,
            params: self.params,
            return_type: self.return_type,
            where_clause: self.where_clause,
            body: self.body.map_prov(f),
            external_kind: self.external_kind,
        }
    }
}

impl<P: Clone + Default, Q: Clone + Default> MapProv<P, Q> for IrImplItem<P> {
    type Output = IrImplItem<Q>;
    fn map_prov(self, f: &impl Fn(P) -> Q) -> IrImplItem<Q> {
        match self {
            IrImplItem::Method(func) => IrImplItem::Method(func.map_prov(f)),
            IrImplItem::AssociatedType { name, ty } => IrImplItem::AssociatedType { name, ty },
        }
    }
}

impl<P: Clone + Default, Q: Clone + Default> MapProv<P, Q> for IrImpl<P> {
    type Output = IrImpl<Q>;
    fn map_prov(self, f: &impl Fn(P) -> Q) -> IrImpl<Q> {
        IrImpl {
            generics: self.generics,
            trait_: self.trait_,
            self_ty: self.self_ty,
            where_clause: self.where_clause,
            items: self.items.into_iter().map(|i| i.map_prov(f)).collect(),
        }
    }
}

impl<F: MapProv<P, Q>, P: Clone + Default, Q: Clone + Default> MapProv<P, Q> for IrModule<F, P> {
    type Output = IrModule<F::Output, Q>;
    /// Transform all provenance annotations in this module from `P` to `Q`.
    ///
    /// This is the primary mechanism for integrating provenance across
    /// compilation stages.  For example, converting circuit-level provenance
    /// into the weaver's output provenance:
    ///
    /// ```ignore
    /// let module: IrModule<IrFunction<CircuitProv>, CircuitProv> = weave(...);
    /// let mapped = module.map_prov(&|cp| AppProv::from(cp));
    /// ```
    fn map_prov(self, f: &impl Fn(P) -> Q) -> IrModule<F::Output, Q> {
        IrModule {
            name: self.name,
            structs: self.structs,
            enums: self.enums,
            traits: self.traits,
            impls: self.impls.into_iter().map(|i| i.map_prov(f)).collect(),
            functions: self.functions.into_iter().map(|func| func.map_prov(f)).collect(),
            type_aliases: self.type_aliases,
            consts: self.consts,
        }
    }
}

// ============================================================================
// CFG IR — control-flow-graph-structured function bodies
// ============================================================================

/// A jump to a target block, carrying block-parameter arguments.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrCfgJump<P: Clone + Default = ()> {
    /// 0-indexed block index within the enclosing [`IrCfgBody`].
    pub target: usize,
    /// Expressions passed as arguments to the target block's params.
    pub args: Vec<IrExpr<P>>,
}

/// Terminates an [`IrCfgBlock`].
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[non_exhaustive]
pub enum IrCfgTerminator<P: Clone + Default = ()> {
    /// Function returns, with an optional value expression.
    Return(Option<IrExpr<P>>),
    /// Unconditional jump to another block.
    Goto(IrCfgJump<P>),
    /// Conditional branch: jumps to `then_` if `cond` is truthy, else `else_`.
    CondGoto {
        cond: IrExpr<P>,
        then_: IrCfgJump<P>,
        else_: IrCfgJump<P>,
    },
}

/// A single basic block in a CFG function body.
///
/// Block 0 is always the entry block.  Block parameters are filled by the
/// `args` of incoming jumps (SSA block-argument style, analogous to φ-nodes).
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrCfgBlock<P: Clone + Default = ()> {
    /// SSA block parameters — bound by the `args` of jumps that target this block.
    pub params: Vec<IrParam>,
    pub stmts: Vec<IrStmt<P>>,
    pub stmt_provs: Vec<P>,
    pub terminator: IrCfgTerminator<P>,
}

/// The body of a CFG-structured function: an ordered list of basic blocks.
///
/// Block 0 is the entry.  Control flows between blocks via terminators.
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrCfgBody<P: Clone + Default = ()> {
    pub blocks: Vec<IrCfgBlock<P>>,
}

/// A function with a CFG-structured body.
///
/// Contrast with [`IrFunction`], whose body is a tree-structured [`IrBlock`].
/// CFG functions are produced by weavers that operate on [`IRBlocks`] directly
/// (e.g., CFG-capable FHE schemes) without first flattening via movfuscation.
///
/// [`IRBlocks`]: volar_ir::ir::IRBlocks
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct IrCfgFunction<P: Clone + Default = ()> {
    pub name: String,
    pub generics: Vec<IrGenericParam>,
    pub receiver: Option<IrReceiver>,
    pub params: Vec<IrParam>,
    pub return_type: Option<IrType>,
    pub where_clause: Vec<IrWherePredicate>,
    pub external_kind: ExternalKind,
    pub body: IrCfgBody<P>,
}

impl<P: Clone + Default, Q: Clone + Default> MapProv<P, Q> for IrCfgJump<P> {
    type Output = IrCfgJump<Q>;
    fn map_prov(self, f: &impl Fn(P) -> Q) -> IrCfgJump<Q> {
        IrCfgJump {
            target: self.target,
            args: self.args.into_iter().map(|e| e.map_prov(f)).collect(),
        }
    }
}

impl<P: Clone + Default, Q: Clone + Default> MapProv<P, Q> for IrCfgTerminator<P> {
    type Output = IrCfgTerminator<Q>;
    fn map_prov(self, f: &impl Fn(P) -> Q) -> IrCfgTerminator<Q> {
        match self {
            IrCfgTerminator::Return(e) => IrCfgTerminator::Return(e.map(|e| e.map_prov(f))),
            IrCfgTerminator::Goto(j) => IrCfgTerminator::Goto(j.map_prov(f)),
            IrCfgTerminator::CondGoto { cond, then_, else_ } => IrCfgTerminator::CondGoto {
                cond: cond.map_prov(f),
                then_: then_.map_prov(f),
                else_: else_.map_prov(f),
            },
        }
    }
}

impl<P: Clone + Default, Q: Clone + Default> MapProv<P, Q> for IrCfgBlock<P> {
    type Output = IrCfgBlock<Q>;
    fn map_prov(self, f: &impl Fn(P) -> Q) -> IrCfgBlock<Q> {
        IrCfgBlock {
            params: self.params,
            stmts: self.stmts.into_iter().map(|s| s.map_prov(f)).collect(),
            stmt_provs: self.stmt_provs.into_iter().map(|p| f(p)).collect(),
            terminator: self.terminator.map_prov(f),
        }
    }
}

impl<P: Clone + Default, Q: Clone + Default> MapProv<P, Q> for IrCfgBody<P> {
    type Output = IrCfgBody<Q>;
    fn map_prov(self, f: &impl Fn(P) -> Q) -> IrCfgBody<Q> {
        IrCfgBody {
            blocks: self.blocks.into_iter().map(|b| b.map_prov(f)).collect(),
        }
    }
}

impl<P: Clone + Default, Q: Clone + Default> MapProv<P, Q> for IrCfgFunction<P> {
    type Output = IrCfgFunction<Q>;
    fn map_prov(self, f: &impl Fn(P) -> Q) -> IrCfgFunction<Q> {
        IrCfgFunction {
            name: self.name,
            generics: self.generics,
            receiver: self.receiver,
            params: self.params,
            return_type: self.return_type,
            where_clause: self.where_clause,
            external_kind: self.external_kind,
            body: self.body.map_prov(f),
        }
    }
}

/// A function that is either flat (tree-structured) or CFG-structured.
///
/// Used as the function type of [`IrCfgModule`], which holds a mix of
/// circuit-level CFG functions and auxiliary flat functions merged from
/// linked specs via [`LinkageSystem::apply_cfg`].
#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub enum IrAnyFunction<P: Clone + Default = ()> {
    Flat(IrFunction<P>),
    Cfg(IrCfgFunction<P>),
}

impl<P: Clone + Default, Q: Clone + Default> MapProv<P, Q> for IrAnyFunction<P> {
    type Output = IrAnyFunction<Q>;
    fn map_prov(self, f: &impl Fn(P) -> Q) -> IrAnyFunction<Q> {
        match self {
            IrAnyFunction::Flat(func) => IrAnyFunction::Flat(func.map_prov(f)),
            IrAnyFunction::Cfg(func) => IrAnyFunction::Cfg(func.map_prov(f)),
        }
    }
}

/// A module whose functions have CFG-structured bodies (possibly mixed with
/// auxiliary flat functions from linked specs).
///
/// Produced by weavers that emit [`IrCfgFunction`]s rather than flat
/// [`IrFunction`]s.  Use [`volar_compiler::printer::print_cfg_module`] to
/// render to Rust source.
pub type IrCfgModule<P = ()> = IrModule<IrAnyFunction<P>, P>;
