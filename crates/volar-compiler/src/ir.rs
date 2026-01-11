//! Intermediate Representation for volar-spec code.
//!
//! This IR is designed to capture the semantics of the Rust code in volar-spec
//! while being easy to transpile to other languages or compile to templates.

/// A unique identifier for types in the IR
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TypeId(pub usize);

/// A unique identifier for functions/methods in the IR
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct FuncId(pub usize);

/// A unique identifier for variables in the IR
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct VarId(pub usize);

/// A unique identifier for struct definitions
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct StructId(pub usize);

/// A unique identifier for trait definitions
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct TraitId(pub usize);

/// A unique identifier for impl blocks
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ImplId(pub usize);

/// A unique identifier for generic type parameters
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct GenericParamId(pub usize);

/// The root IR module containing all definitions
#[derive(Debug, Clone, Default)]
pub struct IrModule {
    pub name: String,
    pub structs: Vec<IrStruct>,
    pub traits: Vec<IrTrait>,
    pub impls: Vec<IrImpl>,
    pub functions: Vec<IrFunction>,
    pub type_aliases: Vec<IrTypeAlias>,
    pub uses: Vec<IrUse>,
}

/// A use/import statement
#[derive(Debug, Clone)]
pub struct IrUse {
    pub path: Vec<String>,
    pub alias: Option<String>,
    pub glob: bool,
}

/// A type alias definition
#[derive(Debug, Clone)]
pub struct IrTypeAlias {
    pub name: String,
    pub generics: Vec<IrGenericParam>,
    pub ty: IrType,
}

/// A struct definition
#[derive(Debug, Clone)]
pub struct IrStruct {
    pub name: String,
    pub generics: Vec<IrGenericParam>,
    pub fields: Vec<IrField>,
    pub is_tuple: bool,
}

/// A struct field
#[derive(Debug, Clone)]
pub struct IrField {
    pub name: String,
    pub ty: IrType,
    pub visibility: Visibility,
}

/// Visibility modifier
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
    Crate,
}

/// A trait definition
#[derive(Debug, Clone)]
pub struct IrTrait {
    pub name: String,
    pub generics: Vec<IrGenericParam>,
    pub super_traits: Vec<IrTraitBound>,
    pub items: Vec<IrTraitItem>,
}

/// An item within a trait definition
#[derive(Debug, Clone)]
pub enum IrTraitItem {
    Method(IrMethodSignature),
    AssociatedType {
        name: String,
        bounds: Vec<IrTraitBound>,
        default: Option<IrType>,
    },
    Const {
        name: String,
        ty: IrType,
        default: Option<IrExpr>,
    },
}

/// A method signature (without body)
#[derive(Debug, Clone)]
pub struct IrMethodSignature {
    pub name: String,
    pub generics: Vec<IrGenericParam>,
    pub receiver: Option<IrReceiver>,
    pub params: Vec<IrParam>,
    pub return_type: Option<IrType>,
    pub where_clause: Vec<IrWherePredicate>,
}

/// The receiver of a method (self, &self, &mut self)
#[derive(Debug, Clone)]
pub enum IrReceiver {
    Value,
    Ref,
    RefMut,
}

/// A function parameter
#[derive(Debug, Clone)]
pub struct IrParam {
    pub name: String,
    pub ty: IrType,
}

/// An impl block
#[derive(Debug, Clone)]
pub struct IrImpl {
    pub generics: Vec<IrGenericParam>,
    pub trait_: Option<IrTraitRef>,
    pub self_ty: IrType,
    pub where_clause: Vec<IrWherePredicate>,
    pub items: Vec<IrImplItem>,
}

/// A reference to a trait
#[derive(Debug, Clone)]
pub struct IrTraitRef {
    pub path: Vec<String>,
    pub type_args: Vec<IrType>,
}

/// An item within an impl block
#[derive(Debug, Clone)]
pub enum IrImplItem {
    Method(IrFunction),
    AssociatedType {
        name: String,
        ty: IrType,
    },
    Const {
        name: String,
        ty: IrType,
        value: IrExpr,
    },
}

/// A function definition
#[derive(Debug, Clone)]
pub struct IrFunction {
    pub name: String,
    pub generics: Vec<IrGenericParam>,
    pub receiver: Option<IrReceiver>,
    pub params: Vec<IrParam>,
    pub return_type: Option<IrType>,
    pub where_clause: Vec<IrWherePredicate>,
    pub body: IrBlock,
}

/// A generic type parameter
#[derive(Debug, Clone)]
pub struct IrGenericParam {
    pub name: String,
    pub bounds: Vec<IrTraitBound>,
    pub default: Option<IrType>,
}

/// A trait bound
#[derive(Debug, Clone)]
pub struct IrTraitBound {
    pub path: Vec<String>,
    pub type_args: Vec<IrType>,
    pub assoc_type_bindings: Vec<(String, IrType)>,
}

/// A where clause predicate
#[derive(Debug, Clone)]
pub enum IrWherePredicate {
    TypeBound {
        ty: IrType,
        bounds: Vec<IrTraitBound>,
    },
    Lifetime {
        name: String,
        bounds: Vec<String>,
    },
}

/// Type representation in the IR
#[derive(Debug, Clone)]
pub enum IrType {
    /// A named type (could be a struct, primitive, or type parameter)
    Path {
        segments: Vec<String>,
        type_args: Vec<IrType>,
    },
    /// A reference type
    Reference {
        mutable: bool,
        elem: Box<IrType>,
    },
    /// A slice type [T]
    Slice(Box<IrType>),
    /// An array type [T; N]
    Array {
        elem: Box<IrType>,
        len: Box<IrExpr>,
    },
    /// A tuple type (T1, T2, ...)
    Tuple(Vec<IrType>),
    /// A function pointer type
    FnPtr {
        params: Vec<IrType>,
        ret: Box<IrType>,
    },
    /// An impl Trait type
    ImplTrait(Vec<IrTraitBound>),
    /// A dyn Trait type
    DynTrait(Vec<IrTraitBound>),
    /// The unit type ()
    Unit,
    /// The never type !
    Never,
    /// Inferred type _
    Infer,
    /// Associated type projection (T::Assoc)
    Projection {
        base: Box<IrType>,
        assoc: String,
        type_args: Vec<IrType>,
    },
}

/// A block of statements
#[derive(Debug, Clone, Default)]
pub struct IrBlock {
    pub stmts: Vec<IrStmt>,
    pub expr: Option<Box<IrExpr>>,
}

/// A statement
#[derive(Debug, Clone)]
pub enum IrStmt {
    /// A let binding
    Let {
        pattern: IrPattern,
        ty: Option<IrType>,
        init: Option<IrExpr>,
    },
    /// An expression statement
    Expr(IrExpr),
    /// A semicolon-terminated expression
    Semi(IrExpr),
    /// An item definition (nested function, struct, etc.)
    Item(IrItemStmt),
}

/// An item that can appear as a statement
#[derive(Debug, Clone)]
pub enum IrItemStmt {
    Fn(IrFunction),
    Struct(IrStruct),
    Const {
        name: String,
        ty: IrType,
        value: IrExpr,
    },
}

/// A pattern for destructuring
#[derive(Debug, Clone)]
pub enum IrPattern {
    /// A simple identifier binding
    Ident {
        mutable: bool,
        name: String,
        subpat: Option<Box<IrPattern>>,
    },
    /// A tuple pattern (a, b, c)
    Tuple(Vec<IrPattern>),
    /// A struct pattern Struct { field: pat, .. }
    Struct {
        path: Vec<String>,
        fields: Vec<(String, IrPattern)>,
        rest: bool,
    },
    /// A tuple struct pattern TupleStruct(a, b)
    TupleStruct {
        path: Vec<String>,
        elems: Vec<IrPattern>,
    },
    /// A slice pattern [a, b, ..]
    Slice(Vec<IrPattern>),
    /// A wildcard pattern _
    Wild,
    /// A literal pattern
    Lit(IrLit),
    /// A reference pattern &pat or &mut pat
    Ref {
        mutable: bool,
        pat: Box<IrPattern>,
    },
    /// An or pattern a | b
    Or(Vec<IrPattern>),
    /// A rest pattern ..
    Rest,
}

/// An expression
#[derive(Debug, Clone)]
pub enum IrExpr {
    /// A literal value
    Lit(IrLit),
    /// A variable or path reference
    Path {
        segments: Vec<String>,
        type_args: Vec<IrType>,
    },
    /// A binary operation
    Binary {
        op: IrBinOp,
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    /// A unary operation
    Unary {
        op: IrUnaryOp,
        expr: Box<IrExpr>,
    },
    /// A function or method call
    Call {
        func: Box<IrExpr>,
        args: Vec<IrExpr>,
    },
    /// A method call
    MethodCall {
        receiver: Box<IrExpr>,
        method: String,
        type_args: Vec<IrType>,
        args: Vec<IrExpr>,
    },
    /// Field access
    Field {
        base: Box<IrExpr>,
        field: String,
    },
    /// Index access
    Index {
        base: Box<IrExpr>,
        index: Box<IrExpr>,
    },
    /// A struct expression
    Struct {
        path: Vec<String>,
        fields: Vec<(String, IrExpr)>,
        rest: Option<Box<IrExpr>>,
    },
    /// A tuple expression
    Tuple(Vec<IrExpr>),
    /// An array expression
    Array(Vec<IrExpr>),
    /// An array repeat expression [expr; len]
    Repeat {
        elem: Box<IrExpr>,
        len: Box<IrExpr>,
    },
    /// A block expression
    Block(IrBlock),
    /// An if expression
    If {
        cond: Box<IrExpr>,
        then_branch: IrBlock,
        else_branch: Option<Box<IrExpr>>,
    },
    /// A match expression
    Match {
        expr: Box<IrExpr>,
        arms: Vec<IrMatchArm>,
    },
    /// A for loop
    ForLoop {
        pattern: IrPattern,
        iter: Box<IrExpr>,
        body: IrBlock,
    },
    /// A while loop
    While {
        cond: Box<IrExpr>,
        body: IrBlock,
    },
    /// A loop
    Loop {
        body: IrBlock,
    },
    /// A break expression
    Break(Option<Box<IrExpr>>),
    /// A continue expression
    Continue,
    /// A return expression
    Return(Option<Box<IrExpr>>),
    /// A closure
    Closure {
        params: Vec<IrClosureParam>,
        ret_type: Option<Box<IrType>>,
        body: Box<IrExpr>,
    },
    /// A reference expression &expr or &mut expr
    Ref {
        mutable: bool,
        expr: Box<IrExpr>,
    },
    /// A dereference expression *expr
    Deref(Box<IrExpr>),
    /// A cast expression expr as Type
    Cast {
        expr: Box<IrExpr>,
        ty: Box<IrType>,
    },
    /// A range expression
    Range {
        start: Option<Box<IrExpr>>,
        end: Option<Box<IrExpr>>,
        inclusive: bool,
    },
    /// An assignment expression
    Assign {
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    /// A compound assignment expression (+=, -=, etc.)
    AssignOp {
        op: IrBinOp,
        left: Box<IrExpr>,
        right: Box<IrExpr>,
    },
    /// A try expression (?)
    Try(Box<IrExpr>),
    /// An await expression
    Await(Box<IrExpr>),
    /// Macro invocation (stored as the macro name and tokens)
    Macro {
        path: Vec<String>,
        tokens: String,
    },
    /// Parenthesized expression
    Paren(Box<IrExpr>),
    /// Unsafe block
    Unsafe(IrBlock),
    /// Let expression (let pat = expr)
    Let {
        pattern: IrPattern,
        expr: Box<IrExpr>,
    },
}

/// A closure parameter
#[derive(Debug, Clone)]
pub struct IrClosureParam {
    pub pattern: IrPattern,
    pub ty: Option<IrType>,
}

/// A match arm
#[derive(Debug, Clone)]
pub struct IrMatchArm {
    pub pattern: IrPattern,
    pub guard: Option<IrExpr>,
    pub body: IrExpr,
}

/// A literal value
#[derive(Debug, Clone)]
pub enum IrLit {
    Int(i128),
    Float(f64),
    Bool(bool),
    Char(char),
    Str(String),
    ByteStr(Vec<u8>),
    Byte(u8),
}

/// Binary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IrBinOp {
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

/// Unary operators
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum IrUnaryOp {
    Neg,
    Not,
    Deref,
}

impl IrModule {
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            name: name.into(),
            ..Default::default()
        }
    }

    pub fn add_struct(&mut self, s: IrStruct) -> StructId {
        let id = StructId(self.structs.len());
        self.structs.push(s);
        id
    }

    pub fn add_trait(&mut self, t: IrTrait) -> TraitId {
        let id = TraitId(self.traits.len());
        self.traits.push(t);
        id
    }

    pub fn add_impl(&mut self, i: IrImpl) -> ImplId {
        let id = ImplId(self.impls.len());
        self.impls.push(i);
        id
    }

    pub fn add_function(&mut self, f: IrFunction) -> FuncId {
        let id = FuncId(self.functions.len());
        self.functions.push(f);
        id
    }
}

impl IrType {
    /// Create a simple path type from a single name
    pub fn simple(name: impl Into<String>) -> Self {
        Self::Path {
            segments: vec![name.into()],
            type_args: vec![],
        }
    }

    /// Create a generic type with type arguments
    pub fn generic(name: impl Into<String>, args: Vec<IrType>) -> Self {
        Self::Path {
            segments: vec![name.into()],
            type_args: args,
        }
    }

    /// Create a reference type
    pub fn reference(mutable: bool, elem: IrType) -> Self {
        Self::Reference {
            mutable,
            elem: Box::new(elem),
        }
    }
}
