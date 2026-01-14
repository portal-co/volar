//! Specialized IR types for volar-spec.
//!
//! This module provides specialized enums that replace string-based representations
//! with domain-specific types for:
//! 1. Math operations (Add, Sub, Mul, etc.)
//! 2. Cryptographic operations (hash, encrypt, etc.)
//! 3. Primitive types (bytes, field elements)
//! 4. Array types (GenericArray, fixed-size arrays)
//! 5. Struct types (Delta, Q, Vope, etc.)

use std::fmt;

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

/// Array-like types used in volar-spec
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ArrayKind {
    /// A GenericArray<T, N> from the `generic-array` crate
    GenericArray,
    /// A fixed-size Rust array [T; N]
    FixedArray,
    /// A slice reference &[T]
    Slice,
}

/// Type-level array length specification
#[derive(Debug, Clone, PartialEq)]
pub enum ArrayLength {
    /// A constant length known at compile time
    Const(usize),
    /// A type parameter representing the length (e.g., N in GenericArray<T, N>)
    TypeParam(String),
    /// A typenum constant (U1, U8, U16, etc.)
    TypeNum(TypeNumConst),
    /// A computed length from an expression
    Computed(Box<SpecExpr>),
}

/// Typenum constants used in volar-spec
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

    pub fn value(&self) -> usize {
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

impl fmt::Display for TypeNumConst {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::U0 => write!(f, "U0"),
            Self::U1 => write!(f, "U1"),
            Self::U2 => write!(f, "U2"),
            Self::U8 => write!(f, "U8"),
            Self::U16 => write!(f, "U16"),
            Self::U32 => write!(f, "U32"),
            Self::U64 => write!(f, "U64"),
        }
    }
}

// ============================================================================
// STRUCT TYPES (Domain-specific structures)
// ============================================================================

/// Known struct types in volar-spec
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum StructKind {
    /// Delta<N, T> - Verifier's secret delta values
    Delta,
    /// Q<N, T> - Verifier's Q values (delta * value)
    Q,
    /// Vope<N, T, K> - VOLE polynomial evaluation (prover side)
    Vope,
    /// Poly<N, T> - Polynomial representation
    Poly,
    /// PolyInputPool<T, N, X> - Pool of polynomial inputs
    PolyInputPool,
    /// BitVole<N, T> - Bit-level VOLE
    BitVole,
    /// ABO<B, D, K> - All-but-one commitment
    ABO,
    /// ABOOpening<B, D, T, U> - Opening of ABO commitment
    ABOOpening,
    /// CommitmentCore<D> - Hash-based commitment
    CommitmentCore,
    /// GenericArray<T, N> - Generic fixed-size array (external)
    GenericArray,
    /// A user-defined struct not in the known set
    Custom(String),
}

impl StructKind {
    pub fn from_str(s: &str) -> Self {
        match s {
            "Delta" => Self::Delta,
            "Q" => Self::Q,
            "Vope" => Self::Vope,
            "Poly" => Self::Poly,
            "PolyInputPool" => Self::PolyInputPool,
            "BitVole" => Self::BitVole,
            "ABO" => Self::ABO,
            "ABOOpening" => Self::ABOOpening,
            "CommitmentCore" => Self::CommitmentCore,
            "GenericArray" => Self::GenericArray,
            other => Self::Custom(other.to_string()),
        }
    }

    /// Check if this is a VOLE-related struct
    pub fn is_vole_struct(&self) -> bool {
        matches!(self, Self::Delta | Self::Q | Self::Vope | Self::BitVole)
    }

    /// Check if this is a cryptographic struct
    pub fn is_crypto_struct(&self) -> bool {
        matches!(self, Self::ABO | Self::ABOOpening | Self::CommitmentCore)
    }
}

impl fmt::Display for StructKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Delta => write!(f, "Delta"),
            Self::Q => write!(f, "Q"),
            Self::Vope => write!(f, "Vope"),
            Self::Poly => write!(f, "Poly"),
            Self::PolyInputPool => write!(f, "PolyInputPool"),
            Self::BitVole => write!(f, "BitVole"),
            Self::ABO => write!(f, "ABO"),
            Self::ABOOpening => write!(f, "ABOOpening"),
            Self::CommitmentCore => write!(f, "CommitmentCore"),
            Self::GenericArray => write!(f, "GenericArray"),
            Self::Custom(name) => write!(f, "{}", name),
        }
    }
}

// ============================================================================
// MATH OPERATION TRAITS
// ============================================================================

/// Mathematical operation traits
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum MathTrait {
    // Arithmetic operations
    /// Addition (a + b)
    Add,
    /// Subtraction (a - b)
    Sub,
    /// Multiplication (a * b)
    Mul,
    /// Division (a / b)
    Div,
    /// Remainder/Modulo (a % b)
    Rem,

    // Bitwise operations
    /// Bitwise AND (a & b)
    BitAnd,
    /// Bitwise OR (a | b)
    BitOr,
    /// Bitwise XOR (a ^ b)
    BitXor,
    /// Left shift (a << b)
    Shl,
    /// Right shift (a >> b)
    Shr,

    // Unary operations
    /// Negation (-a)
    Neg,
    /// Bitwise NOT (!a)
    Not,

    // Comparison
    /// Equality comparison
    PartialEq,
    /// Total equality
    Eq,
    /// Partial ordering
    PartialOrd,
    /// Total ordering
    Ord,

    // Cloning/copying
    /// Clone trait
    Clone,
    /// Copy trait
    Copy,

    // Default
    /// Default trait
    Default,

    // Conversion
    /// Into<T> conversion
    Into,
    /// From<T> conversion
    From,
}

impl MathTrait {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
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
            "Into" => Some(Self::Into),
            "From" => Some(Self::From),
            _ => None,
        }
    }

    /// Check if this is a binary operation
    pub fn is_binary(&self) -> bool {
        matches!(
            self,
            Self::Add
                | Self::Sub
                | Self::Mul
                | Self::Div
                | Self::Rem
                | Self::BitAnd
                | Self::BitOr
                | Self::BitXor
                | Self::Shl
                | Self::Shr
        )
    }

    /// Check if this is a unary operation
    pub fn is_unary(&self) -> bool {
        matches!(self, Self::Neg | Self::Not)
    }

    /// Get the operator symbol for this trait
    pub fn operator_symbol(&self) -> Option<&'static str> {
        match self {
            Self::Add => Some("+"),
            Self::Sub => Some("-"),
            Self::Mul => Some("*"),
            Self::Div => Some("/"),
            Self::Rem => Some("%"),
            Self::BitAnd => Some("&"),
            Self::BitOr => Some("|"),
            Self::BitXor => Some("^"),
            Self::Shl => Some("<<"),
            Self::Shr => Some(">>"),
            Self::Neg => Some("-"),
            Self::Not => Some("!"),
            _ => None,
        }
    }
}

impl fmt::Display for MathTrait {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Add => write!(f, "Add"),
            Self::Sub => write!(f, "Sub"),
            Self::Mul => write!(f, "Mul"),
            Self::Div => write!(f, "Div"),
            Self::Rem => write!(f, "Rem"),
            Self::BitAnd => write!(f, "BitAnd"),
            Self::BitOr => write!(f, "BitOr"),
            Self::BitXor => write!(f, "BitXor"),
            Self::Shl => write!(f, "Shl"),
            Self::Shr => write!(f, "Shr"),
            Self::Neg => write!(f, "Neg"),
            Self::Not => write!(f, "Not"),
            Self::PartialEq => write!(f, "PartialEq"),
            Self::Eq => write!(f, "Eq"),
            Self::PartialOrd => write!(f, "PartialOrd"),
            Self::Ord => write!(f, "Ord"),
            Self::Clone => write!(f, "Clone"),
            Self::Copy => write!(f, "Copy"),
            Self::Default => write!(f, "Default"),
            Self::Into => write!(f, "Into"),
            Self::From => write!(f, "From"),
        }
    }
}

// ============================================================================
// CRYPTOGRAPHIC OPERATION TRAITS
// ============================================================================

/// Cryptographic operation traits
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum CryptoTrait {
    /// Block cipher encryption (BlockEncrypt)
    BlockEncrypt,
    /// Block cipher (generic BlockCipher)
    BlockCipher,
    /// Digest/Hash function
    Digest,
    /// Array length type constraint
    ArrayLength,
    /// VOLE array constraint
    VoleArray,
    /// Byte-based block encryption
    ByteBlockEncrypt,
    /// Field element inversion
    Invert,
    /// Deref trait (for dereferencing)
    Deref,
    /// AsRef trait
    AsRef,
}

impl CryptoTrait {
    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "BlockEncrypt" => Some(Self::BlockEncrypt),
            "BlockCipher" => Some(Self::BlockCipher),
            "Digest" => Some(Self::Digest),
            "ArrayLength" => Some(Self::ArrayLength),
            "VoleArray" => Some(Self::VoleArray),
            "ByteBlockEncrypt" => Some(Self::ByteBlockEncrypt),
            "Invert" => Some(Self::Invert),
            "Deref" => Some(Self::Deref),
            "AsRef" => Some(Self::AsRef),
            _ => None,
        }
    }
}

impl fmt::Display for CryptoTrait {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::BlockEncrypt => write!(f, "BlockEncrypt"),
            Self::BlockCipher => write!(f, "BlockCipher"),
            Self::Digest => write!(f, "Digest"),
            Self::ArrayLength => write!(f, "ArrayLength"),
            Self::VoleArray => write!(f, "VoleArray"),
            Self::ByteBlockEncrypt => write!(f, "ByteBlockEncrypt"),
            Self::Invert => write!(f, "Invert"),
            Self::Deref => write!(f, "Deref"),
            Self::AsRef => write!(f, "AsRef"),
        }
    }
}

// ============================================================================
// UNIFIED TRAIT REFERENCE
// ============================================================================

/// A reference to a trait, either math or crypto
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum TraitKind {
    /// A mathematical operation trait
    Math(MathTrait),
    /// A cryptographic operation trait
    Crypto(CryptoTrait),
    /// A trait from an external crate
    External { path: Vec<String> },
    /// A custom/unknown trait
    Custom(String),
}

impl TraitKind {
    pub fn from_path(segments: &[String]) -> Self {
        let name = segments.last().map(|s| s.as_str()).unwrap_or("");

        // Try math traits first
        if let Some(math) = MathTrait::from_str(name) {
            return Self::Math(math);
        }

        // Try crypto traits
        if let Some(crypto) = CryptoTrait::from_str(name) {
            return Self::Crypto(crypto);
        }

        // Check for known external paths
        if segments.len() > 1 {
            let module = segments.first().map(|s| s.as_str()).unwrap_or("");
            match module {
                "core" | "std" | "cipher" | "digest" | "rand" => {
                    return Self::External {
                        path: segments.to_vec(),
                    };
                }
                _ => {}
            }
        }

        Self::Custom(name.to_string())
    }
}

impl fmt::Display for TraitKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Math(m) => write!(f, "{}", m),
            Self::Crypto(c) => write!(f, "{}", c),
            Self::External { path } => write!(f, "{}", path.join("::")),
            Self::Custom(name) => write!(f, "{}", name),
        }
    }
}

// ============================================================================
// SPECIALIZED TYPE
// ============================================================================

/// A fully specialized type representation
#[derive(Debug, Clone, PartialEq)]
pub enum SpecType {
    /// A primitive type
    Primitive(PrimitiveType),

    /// An array type
    Array {
        kind: ArrayKind,
        elem: Box<SpecType>,
        len: ArrayLength,
    },

    /// A struct type with type arguments
    Struct {
        kind: StructKind,
        type_args: Vec<SpecType>,
    },

    /// A reference type
    Reference {
        mutable: bool,
        elem: Box<SpecType>,
    },

    /// A tuple type
    Tuple(Vec<SpecType>),

    /// Unit type ()
    Unit,

    /// A type parameter (generic)
    TypeParam(String),

    /// An associated type projection (T::Output)
    Projection {
        base: Box<SpecType>,
        assoc: AssociatedType,
    },

    /// An impl Trait type
    ImplTrait(Vec<SpecTraitBound>),

    /// Never type !
    Never,

    /// Inferred type _
    Infer,

    /// A function pointer type
    FnPtr {
        params: Vec<SpecType>,
        ret: Box<SpecType>,
    },
}

impl SpecType {
    /// Check if this type is a primitive
    pub fn is_primitive(&self) -> bool {
        matches!(self, Self::Primitive(_))
    }

    /// Check if this type is an array
    pub fn is_array(&self) -> bool {
        matches!(self, Self::Array { .. })
    }

    /// Check if this type is a struct
    pub fn is_struct(&self) -> bool {
        matches!(self, Self::Struct { .. })
    }

    /// Get the struct kind if this is a struct type
    pub fn as_struct(&self) -> Option<&StructKind> {
        match self {
            Self::Struct { kind, .. } => Some(kind),
            _ => None,
        }
    }
}

// ============================================================================
// ASSOCIATED TYPES
// ============================================================================

/// Known associated types
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssociatedType {
    /// Output type from operator traits
    Output,
    /// BlockSize from cipher traits
    BlockSize,
    /// OutputSize from digest traits
    OutputSize,
    /// Target from Deref
    Target,
    /// A custom associated type
    Custom(String),
}

impl AssociatedType {
    pub fn from_str(s: &str) -> Self {
        match s {
            "Output" => Self::Output,
            "BlockSize" => Self::BlockSize,
            "OutputSize" => Self::OutputSize,
            "Target" => Self::Target,
            other => Self::Custom(other.to_string()),
        }
    }
}

impl fmt::Display for AssociatedType {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Output => write!(f, "Output"),
            Self::BlockSize => write!(f, "BlockSize"),
            Self::OutputSize => write!(f, "OutputSize"),
            Self::Target => write!(f, "Target"),
            Self::Custom(name) => write!(f, "{}", name),
        }
    }
}

// ============================================================================
// TRAIT BOUNDS
// ============================================================================

/// A specialized trait bound
#[derive(Debug, Clone, PartialEq)]
pub struct SpecTraitBound {
    pub trait_kind: TraitKind,
    pub type_args: Vec<SpecType>,
    pub assoc_bindings: Vec<(AssociatedType, SpecType)>,
}

// ============================================================================
// SPECIALIZED EXPRESSIONS
// ============================================================================

/// Specialized binary operation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpecBinOp {
    /// Arithmetic operations
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    /// Bitwise operations
    BitAnd,
    BitOr,
    BitXor,
    Shl,
    Shr,
    /// Comparison operations
    Eq,
    Ne,
    Lt,
    Le,
    Gt,
    Ge,
    /// Logical operations
    And,
    Or,
}

impl SpecBinOp {
    pub fn to_trait(&self) -> Option<MathTrait> {
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
            _ => None,
        }
    }

    pub fn symbol(&self) -> &'static str {
        match self {
            Self::Add => "+",
            Self::Sub => "-",
            Self::Mul => "*",
            Self::Div => "/",
            Self::Rem => "%",
            Self::BitAnd => "&",
            Self::BitOr => "|",
            Self::BitXor => "^",
            Self::Shl => "<<",
            Self::Shr => ">>",
            Self::Eq => "==",
            Self::Ne => "!=",
            Self::Lt => "<",
            Self::Le => "<=",
            Self::Gt => ">",
            Self::Ge => ">=",
            Self::And => "&&",
            Self::Or => "||",
        }
    }
}

/// Specialized unary operation
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpecUnaryOp {
    Neg,
    Not,
    Deref,
}

impl SpecUnaryOp {
    pub fn to_trait(&self) -> Option<MathTrait> {
        match self {
            Self::Neg => Some(MathTrait::Neg),
            Self::Not => Some(MathTrait::Not),
            Self::Deref => None,
        }
    }

    pub fn symbol(&self) -> &'static str {
        match self {
            Self::Neg => "-",
            Self::Not => "!",
            Self::Deref => "*",
        }
    }
}

/// Known methods on VOLE types
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VoleMethod {
    /// Remap array indices
    Remap,
    /// Rotate left by element count
    RotateLeft,
    /// Rotate right by element count
    RotateRight,
    /// Rotate left by bit count
    RotateLeftBits,
    /// Rotate right by bit count
    RotateRightBits,
    /// Extract a single bit
    Bit,
    /// Expand to larger degree
    Expand,
    /// Scale values
    Scale,
    /// Create static Q from Delta
    Static,
    /// Clone the value
    Clone,
    /// Get Q values from polynomial
    GetQs,
    /// Get Q values from polynomial with pool
    GetQsPool,
    /// Apply polynomial
    Apply,
    /// Apply polynomial with pool
    ApplyPool,
    /// Zip two arrays
    Zip,
    /// Generate array from function
    Generate,
    /// Fold/reduce array
    Fold,
    /// Map over array
    Map,
    /// Iterator method
    Iter,
    /// Enumerate iterator
    Enumerate,
    /// To VOLE material
    ToVoleMaterial,
    /// To VOLE material with typenum
    ToVoleMaterialTypenum,
    /// A custom method
    Custom(String),
}

impl VoleMethod {
    pub fn from_str(s: &str) -> Self {
        match s {
            "remap" => Self::Remap,
            "rotate_left" => Self::RotateLeft,
            "rotate_right" => Self::RotateRight,
            "rotate_left_bits" => Self::RotateLeftBits,
            "rotate_right_bits" => Self::RotateRightBits,
            "bit" => Self::Bit,
            "expand" => Self::Expand,
            "scale" => Self::Scale,
            "r#static" | "static" => Self::Static,
            "clone" => Self::Clone,
            "get_qs" => Self::GetQs,
            "get_qs_pool" => Self::GetQsPool,
            "apply" => Self::Apply,
            "apply_pool" => Self::ApplyPool,
            "zip" => Self::Zip,
            "generate" => Self::Generate,
            "fold" => Self::Fold,
            "map" => Self::Map,
            "iter" => Self::Iter,
            "enumerate" => Self::Enumerate,
            "to_vole_material" => Self::ToVoleMaterial,
            "to_vole_material_typenum" => Self::ToVoleMaterialTypenum,
            other => Self::Custom(other.to_string()),
        }
    }
}

impl fmt::Display for VoleMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Remap => write!(f, "remap"),
            Self::RotateLeft => write!(f, "rotate_left"),
            Self::RotateRight => write!(f, "rotate_right"),
            Self::RotateLeftBits => write!(f, "rotate_left_bits"),
            Self::RotateRightBits => write!(f, "rotate_right_bits"),
            Self::Bit => write!(f, "bit"),
            Self::Expand => write!(f, "expand"),
            Self::Scale => write!(f, "scale"),
            Self::Static => write!(f, "r#static"),
            Self::Clone => write!(f, "clone"),
            Self::GetQs => write!(f, "get_qs"),
            Self::GetQsPool => write!(f, "get_qs_pool"),
            Self::Apply => write!(f, "apply"),
            Self::ApplyPool => write!(f, "apply_pool"),
            Self::Zip => write!(f, "zip"),
            Self::Generate => write!(f, "generate"),
            Self::Fold => write!(f, "fold"),
            Self::Map => write!(f, "map"),
            Self::Iter => write!(f, "iter"),
            Self::Enumerate => write!(f, "enumerate"),
            Self::ToVoleMaterial => write!(f, "to_vole_material"),
            Self::ToVoleMaterialTypenum => write!(f, "to_vole_material_typenum"),
            Self::Custom(name) => write!(f, "{}", name),
        }
    }
}

/// Crypto-specific methods
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CryptoMethod {
    /// Encrypt a block
    EncryptBlock,
    /// Generate ABO commitment
    GenAbo,
    /// Open a commitment
    Open,
    /// Validate a commitment
    Validate,
    /// Commit to a value
    Commit,
    /// Update hash state
    Update,
    /// Finalize hash
    Finalize,
    /// A custom method
    Custom(String),
}

impl CryptoMethod {
    pub fn from_str(s: &str) -> Self {
        match s {
            "encrypt_block" => Self::EncryptBlock,
            "gen_abo" => Self::GenAbo,
            "open" => Self::Open,
            "validate" => Self::Validate,
            "commit" => Self::Commit,
            "update" => Self::Update,
            "finalize" => Self::Finalize,
            other => Self::Custom(other.to_string()),
        }
    }
}

impl fmt::Display for CryptoMethod {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::EncryptBlock => write!(f, "encrypt_block"),
            Self::GenAbo => write!(f, "gen_abo"),
            Self::Open => write!(f, "open"),
            Self::Validate => write!(f, "validate"),
            Self::Commit => write!(f, "commit"),
            Self::Update => write!(f, "update"),
            Self::Finalize => write!(f, "finalize"),
            Self::Custom(name) => write!(f, "{}", name),
        }
    }
}

/// A method call, categorized by domain
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MethodKind {
    /// VOLE-related method
    Vole(VoleMethod),
    /// Cryptographic method
    Crypto(CryptoMethod),
    /// Standard library method
    Std(String),
    /// Unknown method
    Unknown(String),
}

impl MethodKind {
    pub fn from_str(s: &str) -> Self {
        // Check for standard library methods FIRST (these are most common)
        match s {
            "clone" | "default" | "into" | "from" | "as_ref" | "as_slice" | "get" | "len"
            | "is_empty" | "contains" | "unwrap" | "unwrap_or" | "unwrap_or_default" | "expect"
            | "ok" | "err" | "and_then" | "or_else" | "to_usize" | "to_string"
            | "as_ptr" | "wrapping_add" | "wrapping_sub" | "checked_add" | "checked_sub"
            | "saturating_add" | "saturating_sub" | "bitxor" | "deref" => {
                return Self::Std(s.to_string());
            }
            _ => {}
        }

        // Try VOLE methods
        match VoleMethod::from_str(s) {
            VoleMethod::Custom(_) => {}
            vole => return Self::Vole(vole),
        }

        // Try crypto methods
        match CryptoMethod::from_str(s) {
            CryptoMethod::Custom(_) => {}
            crypto => return Self::Crypto(crypto),
        }

        // Anything else is unknown
        Self::Unknown(s.to_string())
    }
}

// ============================================================================
// SPECIALIZED EXPRESSIONS
// ============================================================================

/// A specialized literal value
#[derive(Debug, Clone, PartialEq)]
pub enum SpecLit {
    Int(i128),
    Float(f64),
    Bool(bool),
    Char(char),
    Str(String),
    ByteStr(Vec<u8>),
    Byte(u8),
}

/// A specialized expression
#[derive(Debug, Clone, PartialEq)]
pub enum SpecExpr {
    /// A literal value
    Lit(SpecLit),

    /// A variable reference
    Var(String),

    /// A path expression (possibly with type args)
    Path {
        segments: Vec<String>,
        type_args: Vec<SpecType>,
    },

    /// A binary operation
    Binary {
        op: SpecBinOp,
        left: Box<SpecExpr>,
        right: Box<SpecExpr>,
    },

    /// A unary operation
    Unary {
        op: SpecUnaryOp,
        expr: Box<SpecExpr>,
    },

    /// A method call
    MethodCall {
        receiver: Box<SpecExpr>,
        method: MethodKind,
        type_args: Vec<SpecType>,
        args: Vec<SpecExpr>,
    },

    /// A function call
    Call {
        func: Box<SpecExpr>,
        args: Vec<SpecExpr>,
    },

    /// Field access
    Field {
        base: Box<SpecExpr>,
        field: String,
    },

    /// Index access
    Index {
        base: Box<SpecExpr>,
        index: Box<SpecExpr>,
    },

    /// Struct construction
    StructExpr {
        kind: StructKind,
        type_args: Vec<SpecType>,
        fields: Vec<(String, SpecExpr)>,
        rest: Option<Box<SpecExpr>>,
    },

    /// Tuple expression
    Tuple(Vec<SpecExpr>),

    /// Array expression
    Array(Vec<SpecExpr>),

    /// Array repeat expression [expr; len]
    Repeat {
        elem: Box<SpecExpr>,
        len: Box<SpecExpr>,
    },

    /// Block expression
    Block(SpecBlock),

    /// If expression
    If {
        cond: Box<SpecExpr>,
        then_branch: SpecBlock,
        else_branch: Option<Box<SpecExpr>>,
    },

    /// For loop
    ForLoop {
        pattern: SpecPattern,
        iter: Box<SpecExpr>,
        body: SpecBlock,
    },

    /// While loop
    While {
        cond: Box<SpecExpr>,
        body: SpecBlock,
    },

    /// Infinite loop
    Loop {
        body: SpecBlock,
    },

    /// Match expression
    Match {
        expr: Box<SpecExpr>,
        arms: Vec<SpecMatchArm>,
    },

    /// Closure
    Closure {
        params: Vec<SpecClosureParam>,
        ret_type: Option<Box<SpecType>>,
        body: Box<SpecExpr>,
    },

    /// Reference expression
    Ref {
        mutable: bool,
        expr: Box<SpecExpr>,
    },

    /// Cast expression
    Cast {
        expr: Box<SpecExpr>,
        ty: Box<SpecType>,
    },

    /// Return expression
    Return(Option<Box<SpecExpr>>),

    /// Break expression
    Break(Option<Box<SpecExpr>>),

    /// Continue expression
    Continue,

    /// Assignment
    Assign {
        left: Box<SpecExpr>,
        right: Box<SpecExpr>,
    },

    /// Compound assignment
    AssignOp {
        op: SpecBinOp,
        left: Box<SpecExpr>,
        right: Box<SpecExpr>,
    },

    /// Range expression
    Range {
        start: Option<Box<SpecExpr>>,
        end: Option<Box<SpecExpr>>,
        inclusive: bool,
    },

    /// Macro invocation
    Macro {
        name: String,
        tokens: String,
    },

    /// Try expression (?)
    Try(Box<SpecExpr>),
}

/// A specialized pattern
#[derive(Debug, Clone, PartialEq)]
pub enum SpecPattern {
    /// Identifier binding
    Ident {
        mutable: bool,
        name: String,
        subpat: Option<Box<SpecPattern>>,
    },
    /// Tuple pattern
    Tuple(Vec<SpecPattern>),
    /// Struct pattern
    Struct {
        kind: StructKind,
        fields: Vec<(String, SpecPattern)>,
        rest: bool,
    },
    /// Tuple struct pattern
    TupleStruct {
        kind: StructKind,
        elems: Vec<SpecPattern>,
    },
    /// Wildcard
    Wild,
    /// Literal pattern
    Lit(SpecLit),
    /// Reference pattern
    Ref {
        mutable: bool,
        pat: Box<SpecPattern>,
    },
    /// Or pattern
    Or(Vec<SpecPattern>),
    /// Slice pattern
    Slice(Vec<SpecPattern>),
    /// Rest pattern (..)
    Rest,
}

/// A block of statements
#[derive(Debug, Clone, PartialEq, Default)]
pub struct SpecBlock {
    pub stmts: Vec<SpecStmt>,
    pub expr: Option<Box<SpecExpr>>,
}

/// A statement
#[derive(Debug, Clone, PartialEq)]
pub enum SpecStmt {
    /// Let binding
    Let {
        pattern: SpecPattern,
        ty: Option<SpecType>,
        init: Option<SpecExpr>,
    },
    /// Expression statement
    Expr(SpecExpr),
    /// Semicolon-terminated expression
    Semi(SpecExpr),
}

/// A match arm
#[derive(Debug, Clone, PartialEq)]
pub struct SpecMatchArm {
    pub pattern: SpecPattern,
    pub guard: Option<SpecExpr>,
    pub body: SpecExpr,
}

/// A closure parameter
#[derive(Debug, Clone, PartialEq)]
pub struct SpecClosureParam {
    pub pattern: SpecPattern,
    pub ty: Option<SpecType>,
}

// ============================================================================
// SPECIALIZED ITEMS
// ============================================================================

/// A generic parameter
#[derive(Debug, Clone, PartialEq)]
pub struct SpecGenericParam {
    pub name: String,
    pub bounds: Vec<SpecTraitBound>,
    pub default: Option<SpecType>,
}

/// A where predicate
#[derive(Debug, Clone, PartialEq)]
pub enum SpecWherePredicate {
    TypeBound {
        ty: SpecType,
        bounds: Vec<SpecTraitBound>,
    },
}

/// A function parameter
#[derive(Debug, Clone, PartialEq)]
pub struct SpecParam {
    pub name: String,
    pub ty: SpecType,
}

/// Receiver type
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SpecReceiver {
    Value,
    Ref,
    RefMut,
}

/// A specialized function
#[derive(Debug, Clone, PartialEq)]
pub struct SpecFunction {
    pub name: String,
    pub generics: Vec<SpecGenericParam>,
    pub receiver: Option<SpecReceiver>,
    pub params: Vec<SpecParam>,
    pub return_type: Option<SpecType>,
    pub where_clause: Vec<SpecWherePredicate>,
    pub body: SpecBlock,
}

/// A specialized struct field
#[derive(Debug, Clone, PartialEq)]
pub struct SpecField {
    pub name: String,
    pub ty: SpecType,
    pub public: bool,
}

/// A specialized struct definition
#[derive(Debug, Clone, PartialEq)]
pub struct SpecStruct {
    pub kind: StructKind,
    pub generics: Vec<SpecGenericParam>,
    pub fields: Vec<SpecField>,
    pub is_tuple: bool,
}

/// A specialized trait definition
#[derive(Debug, Clone, PartialEq)]
pub struct SpecTrait {
    pub kind: TraitKind,
    pub generics: Vec<SpecGenericParam>,
    pub super_traits: Vec<SpecTraitBound>,
    pub items: Vec<SpecTraitItem>,
}

/// A trait item
#[derive(Debug, Clone, PartialEq)]
pub enum SpecTraitItem {
    Method(SpecMethodSig),
    AssociatedType {
        name: AssociatedType,
        bounds: Vec<SpecTraitBound>,
        default: Option<SpecType>,
    },
}

/// A method signature
#[derive(Debug, Clone, PartialEq)]
pub struct SpecMethodSig {
    pub name: String,
    pub generics: Vec<SpecGenericParam>,
    pub receiver: Option<SpecReceiver>,
    pub params: Vec<SpecParam>,
    pub return_type: Option<SpecType>,
    pub where_clause: Vec<SpecWherePredicate>,
}

/// A specialized impl block
#[derive(Debug, Clone, PartialEq)]
pub struct SpecImpl {
    pub generics: Vec<SpecGenericParam>,
    pub trait_: Option<SpecTraitRef>,
    pub self_ty: SpecType,
    pub where_clause: Vec<SpecWherePredicate>,
    pub items: Vec<SpecImplItem>,
}

/// A trait reference in an impl
#[derive(Debug, Clone, PartialEq)]
pub struct SpecTraitRef {
    pub kind: TraitKind,
    pub type_args: Vec<SpecType>,
}

/// An impl item
#[derive(Debug, Clone, PartialEq)]
pub enum SpecImplItem {
    Method(SpecFunction),
    AssociatedType {
        name: AssociatedType,
        ty: SpecType,
    },
}

/// A specialized module
#[derive(Debug, Clone, PartialEq, Default)]
pub struct SpecModule {
    pub name: String,
    pub structs: Vec<SpecStruct>,
    pub traits: Vec<SpecTrait>,
    pub impls: Vec<SpecImpl>,
    pub functions: Vec<SpecFunction>,
}

impl SpecModule {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ..Default::default()
        }
    }
}
