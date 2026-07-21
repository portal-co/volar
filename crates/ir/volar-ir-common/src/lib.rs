#![no_std]
// @reliability: normal
// @ai: assisted

extern crate alloc;

pub mod complexity;
pub use complexity::{MeasureSpec, ReentryHint, StructRef};

use alloc::{collections::btree_map::BTreeMap, string::String, vec::Vec};

/// Primitive (non-compound) types shared across Volar IR and VAFFLE.
///
/// Compound types (`Vec`, `Tuple`, `Block`, `Func`) are expressed by
/// [`IrType`] and referenced via [`TypeId`].
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[non_exhaustive]
pub enum Type {
    /// Single GF(2) element (one bit).
    Bit,
    /// 8-bit integer / byte.
    _8,
    /// 16-bit integer.
    _16,
    /// 32-bit integer.
    _32,
    /// 64-bit integer.
    _64,
    /// 128-bit integer.
    _128,
    /// 256-bit value (e.g. full AES lane).
    _256,
    /// GF(2^8) element via the AES polynomial.
    AES8,
    /// GF(2^64) field element.
    Galois64,
    /// GF(3) element — mod-3 integer stored in 2 bits.
    ///
    /// Used by the TFHE backend; not valid in `lower_ir_to_boolar` (GF(2)-only).
    Z3,
}

/// A 256-bit compile-time constant, split into high and low 128-bit halves.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[cfg_attr(feature = "rkyv", rkyv(attr(derive(PartialEq, Eq, PartialOrd, Ord))))]
pub struct Constant {
    pub hi: u128,
    pub lo: u128,
}

// ============================================================================
// Unified type system
// ============================================================================

/// An opaque index into a [`TypeTable`].
///
/// Both Volar IR (`IRTypeId`) and VAFFLE use this; the former is now just a
/// re-export alias in `volar-ir`.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct TypeId(pub u32);

/// The full IR type language, shared between Volar IR and VAFFLE.
///
/// Primitive scalars are wrapped in [`Primitive`](IrType::Primitive) so that
/// the [`Type`] enum remains the single source of truth for leaf types.
/// Compound types are recursive via [`TypeId`] references into a [`TypeTable`].
///
/// # Variants unique to VAFFLE
/// [`Func`](IrType::Func) represents a first-class function type (used for
/// imports, exports, and higher-order values in VAFFLE modules).
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[non_exhaustive]
pub enum IrType {
    /// A primitive scalar type (bit, integer, or Galois-field element).
    Primitive(Type),
    /// A fixed-length homogeneous vector: `[element; len]`.
    Vec(usize, TypeId),
    /// A heterogeneous product type (tuple).
    Tuple(alloc::vec::Vec<TypeId>),
    /// A block / continuation type: a control-flow label that accepts the
    /// listed parameter types.  Used by Volar IR's `Block`-typed SSA params
    /// and dynamic jump targets.
    Block {
        params: alloc::vec::Vec<TypeId>,
    },
    /// A function type: a callable with the given parameter and result types.
    /// Present in VAFFLE for import/export declarations and first-class
    /// function values.  Not used by Volar IR (which represents functions via
    /// `IRBlocks` rather than typed values).
    Func {
        params: alloc::vec::Vec<TypeId>,
        results: alloc::vec::Vec<TypeId>,
    },
}

/// An interning table for [`IrType`] values.
///
/// Both Volar IR and VAFFLE carry one of these (Volar IR as `IRTypes`, VAFFLE
/// as `Module::types`).  [`TypeId`]s are valid only within the table that
/// produced them.
///
/// # Deduplication
/// [`intern`](TypeTable::intern) does a linear scan for an existing entry
/// before pushing.  Type tables are typically small (tens of entries), so
/// this is acceptable; for large tables consider a separate index.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct TypeTable(pub alloc::vec::Vec<IrType>);

impl TypeTable {
    /// Create an empty table.
    pub fn new() -> Self {
        TypeTable(alloc::vec::Vec::new())
    }

    /// Push `ty` without deduplication and return its new [`TypeId`].
    pub fn push(&mut self, ty: IrType) -> TypeId {
        let id = TypeId(self.0.len() as u32);
        self.0.push(ty);
        id
    }

    /// Look for an existing entry equal to `ty`; if not found, push it.
    /// Returns the [`TypeId`] of the (found or newly inserted) entry.
    pub fn intern(&mut self, ty: IrType) -> TypeId {
        if let Some(pos) = self.0.iter().position(|t| t == &ty) {
            TypeId(pos as u32)
        } else {
            self.push(ty)
        }
    }

    /// Convenience: intern `IrType::Primitive(ty)`.
    pub fn primitive(&mut self, ty: Type) -> TypeId {
        self.intern(IrType::Primitive(ty))
    }

    /// Convenience: intern the `Bit` primitive type.
    pub fn bit(&mut self) -> TypeId {
        self.primitive(Type::Bit)
    }

    /// Return `true` if `id` resolves to `IrType::Primitive(Type::Bit)`.
    pub fn is_bit(&self, id: TypeId) -> bool {
        matches!(self.0.get(id.0 as usize), Some(IrType::Primitive(Type::Bit)))
    }

    /// Return `true` if `id` resolves to `IrType::Block { .. }`.
    pub fn is_block(&self, id: TypeId) -> bool {
        matches!(self.0.get(id.0 as usize), Some(IrType::Block { .. }))
    }
}

impl Default for TypeTable {
    fn default() -> Self {
        Self::new()
    }
}

// ============================================================================
// External-primitive declarations (shared by Volar IR and VAFFLE)
// ============================================================================

/// Declaration of a named pure oracle.
///
/// An oracle is a deterministic external function evaluated by all parties.
/// Its implementation is provided by the execution environment at protocol time.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct OracleDecl {
    pub name: alloc::string::String,
    /// Parameter types in order.
    pub params: alloc::vec::Vec<TypeId>,
    /// Return types in order (length ≥ 1).
    pub results: alloc::vec::Vec<TypeId>,
}

/// Declaration of a named conditional action.
///
/// An action is a side-effectful external function invoked by one party
/// (prover / evaluator) only when a boolean guard is 1.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct ActionDecl {
    pub name: alloc::string::String,
    /// Parameter types in order.
    pub params: alloc::vec::Vec<TypeId>,
    /// Return types in order (length ≥ 1).
    pub results: alloc::vec::Vec<TypeId>,
}

/// Declaration of a named RNG source.
///
/// An RNG source is a zero-argument external function that produces a fresh
/// random value of `ty` on each call.  Unlike `rand`, it is modeled as a named
/// external primitive so that the compiler can emit a call to a named function
/// rather than depending on any specific RNG crate.
///
/// Each [`Stmt::Rng`] references an `RngDecl` by name.  The execution
/// environment supplies the concrete implementation.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct RngDecl {
    pub name: alloc::string::String,
    /// Type of the fresh random value produced on each call.
    pub ty: TypeId,
}

/// Declaration of a static instruction-group kind.
///
/// Instruction groups annotate a validated, lexically nested region of ordinary
/// IR operations. They are compiler metadata rather than runtime calls.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct InstructionGroupDecl {
    /// Host-configured name of this group kind.
    pub name: String,
    /// Captured-input types in declaration order.
    pub params: Vec<TypeId>,
    /// Whether the group must be consumed before a lossy lowering boundary.
    pub disposition: GroupDisposition,
}

/// Compiler contract for an instruction-group declaration.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[non_exhaustive]
pub enum GroupDisposition {
    /// A consumer may explicitly erase this group after declining to specialise it.
    Advisory,
    /// A consumer must validate and lower this group before movfuscation.
    MustConsumeBeforeMovfuscation,
}

/// Identity of one instruction-group declaration in a module table.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct InstructionGroupDeclId(pub u32);

/// Module-unique identity of one static begin/end group instance.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct InstructionGroupId(pub u32);

/// A declared instruction-group instance and its captured typed inputs.
///
/// `V` is representation-specific: VAFFLE stores packed values, while Volar
/// IR stores block-qualified SSA references.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct InstructionGroupInstance<V> {
    /// Module-unique static instance identity.
    pub id: InstructionGroupId,
    /// Declared group kind.
    pub decl: InstructionGroupDeclId,
    /// Typed values captured by the begin marker.
    pub inputs: Vec<V>,
}

impl<V> InstructionGroupInstance<V> {
    /// Fallibly map the instance's captured value references without changing
    /// its identity or declaration.
    pub fn map_inputs<U, E>(
        self,
        f: impl FnMut(V) -> Result<U, E>,
    ) -> Result<InstructionGroupInstance<U>, E> {
        Ok(InstructionGroupInstance {
            id: self.id,
            decl: self.decl,
            inputs: self.inputs.into_iter().map(f).collect::<Result<Vec<_>, _>>()?,
        })
    }
}

// ============================================================================
// Shared statement type
// ============================================================================

/// Identifies one of potentially many independent storage spaces.
///
/// `StorageId(0)` is the "default" storage.  Higher IDs may be used for
/// separate stacks, heaps, or per-type scratch spaces.  The execution
/// environment maps each `StorageId` to a concrete address space.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct StorageId(pub u32);

impl StorageId {
    /// The default / "main" storage space.
    pub const DEFAULT: StorageId = StorageId(0);
    /// A dedicated call-stack storage space.
    pub const STACK: StorageId = StorageId(1);
    /// Storage space for the virtualisation pass bytecode table (see
    /// `volar-ir-virt`).  Chosen to be outside the WASM memory range so
    /// virtualised modules can still reference any `memory(i)`.
    pub const VIRT_BYTECODE: StorageId = StorageId(2);
    /// Base of the reserved range for the virtualisation pass
    /// *register file*.  Each IR type used as a block-param or
    /// terminator-arg type is assigned its own `StorageId` in the range
    /// `[VIRT_REGISTERS_BASE, VIRT_REGISTERS_BASE + n_types)`.  Register
    /// indices live at the address level within that storage.
    pub const VIRT_REGISTERS_BASE: u32 = 3;
    /// Base ID for WASM linear memories.  Memory `i` uses `StorageId(MEMORY_BASE + i)`.
    pub const MEMORY_BASE: u32 = 16;
    /// Convenience: StorageId for WASM memory index `i`.
    pub const fn memory(i: u32) -> StorageId { StorageId(Self::MEMORY_BASE + i) }
    /// Dedicated scratch space for `vaffle_ssa`'s own cross-block value
    /// spilling (`crates/ir/volar-vaffle-target/src/vaffle_ssa.rs`).
    /// Addressed directly by the spilled VAFFLE `ValueId` itself (not
    /// SP-relative, no frame-layout coordination needed) — chosen well
    /// outside the WASM memory range (like [`VIRT_BYTECODE`]) so a module
    /// with any realistic number of declared memories can't collide with
    /// it.
    pub const VAFFLE_SSA_SPILL: StorageId = StorageId(1_000_000);
}

/// A contiguous run of pre-initialised typed elements for a storage space.
///
/// Represents WASM active data-segment initialisation: at module instantiation,
/// before any code runs, cells `offset .. offset + data.len()` of the storage
/// identified by `(storage, ty)` are set to the corresponding `data` values.
///
/// Multiple segments may share the same `StorageId` but have different `TypeId`s
/// (different element-width lanes of the same logical storage).
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct PreInitSegment {
    /// Which storage space to initialise.
    pub storage: StorageId,
    /// Element type — indexes the containing module's `TypeTable`.
    /// Multiple segments for the same `StorageId` may use different `TypeId`s.
    pub ty: TypeId,
    /// Element offset of `data[0]` within the `(storage, ty)` lane.
    /// For WASM byte-typed memories this equals the WASM byte offset.
    pub offset: usize,
    /// Typed initial values — one `Constant` per element.
    /// `hi = 0` for types narrower than 128 bits; `lo` holds the value.
    /// Can be bit-packed by the consumer; prefer the typed accessors below.
    pub data: alloc::vec::Vec<Constant>,
}

impl PreInitSegment {
    /// Value of element `i` as a `u8`.
    pub fn as_u8(&self, i: usize) -> u8 { self.data[i].lo as u8 }
    /// Value of element `i` as a `u16`.
    pub fn as_u16(&self, i: usize) -> u16 { self.data[i].lo as u16 }
    /// Value of element `i` as a `u32`.
    pub fn as_u32(&self, i: usize) -> u32 { self.data[i].lo as u32 }
    /// Value of element `i` as a `u64`.
    pub fn as_u64(&self, i: usize) -> u64 { self.data[i].lo as u64 }
    /// Value of element `i` as a `u128`.
    pub fn as_u128(&self, i: usize) -> u128 { self.data[i].lo }
    /// Full 256-bit `Constant` for element `i`.
    pub fn as_constant(&self, i: usize) -> Constant { self.data[i] }
    /// Absolute cell index for element `i`: `self.offset + i`.
    pub fn cell_index(&self, i: usize) -> usize { self.offset + i }
}

/// Shared computational statement type for Volar IR and VAFFLE.
///
/// Generic over four parameters:
///
/// | Parameter | Volar IR        | VAFFLE      | Default     |
/// |-----------|-----------------|-------------|-------------|
/// | `Var`     | `IRVarId`       | `ValueId`   | (required)  |
/// | `Addr`    | `IRVarId`       | `ValueId`   | `Var`       |
/// | `Ty`      | `TypeId`        | `TypeId`    | `TypeId`    |
/// | `Stor`    | `StorageId`     | `StorageId` | `StorageId` |
///
/// The `Ty` and `Stor` parameters allow transformations (e.g. type-table
/// remapping, storage relabelling) to be expressed as a single [`Stmt::map`]
/// call.  Existing usages `Stmt<IRVarId>` continue to compile unchanged.
///
/// # Invariants
/// * `Poly.coeffs` — monomial keys must be sorted (no duplicates within a key).
/// * `Shuffle.result_bits` — each `(bit_idx, var)` selects bit `bit_idx`
///   from `var`; together they define every bit of the output, LSB first.
///   Length equals the output bit-width.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[non_exhaustive]
pub enum Stmt<Var, Addr = Var, Ty = TypeId, Stor = StorageId> {
    /// Load a value from a storage location addressed by `addr`.
    StorageRead {
        storage: Stor,
        ty: Ty,
        addr: Addr,
    },
    /// Write `src` to the storage location addressed by `addr`.
    StorageWrite {
        storage: Stor,
        src: Var,
        ty: Ty,
        addr: Addr,
    },
    /// A compile-time constant value of type `ty`.
    Const(Constant, Ty),
    /// Reinterpret `src` (of type `src_ty`) as `dst_ty` without changing bits.
    Transmute {
        src: Var,
        src_ty: Ty,
        dst_ty: Ty,
    },
    /// Multivariate polynomial over variables.
    ///
    /// Each `(monomial, coeff)` contributes `coeff * product(monomial)` to
    /// the sum; `constant` is the degree-0 term.  Monomial keys must be
    /// sorted.
    ///
    /// # Type semantics
    /// * If `ty` resolves to `Bit`: all variables must be `Bit`; arithmetic
    ///   is GF(2) (mod 2 on every coefficient bit).
    /// * If `ty` resolves to a bitvector or field element `T`: at most one
    ///   variable across all monomials may have type `T` (the "non-Bit slot");
    ///   all other variables in that monomial must be `Bit` and act as GF(2)
    ///   selectors.  The constant term occupies the lowest `bits(T)` bits of
    ///   `constant`.  Mixing two distinct non-GF(2) field types is prohibited.
    Poly {
        /// Output (and dominant operand) type.
        ty: Ty,
        #[cfg_attr(feature = "rkyv", rkyv(with = rkyv::with::AsVec))]
        coeffs: BTreeMap<Vec<Var>, u8>,
        constant: Constant,
    },
    /// Rotate-left `src` (of type `ty`) by `n` bit positions.
    Rol {
        src: Var,
        ty: Ty,
        n: usize,
    },
    /// Rotate-right `src` (of type `ty`) by `n` bit positions.
    Ror {
        src: Var,
        ty: Ty,
        n: usize,
    },
    /// Concatenate `parts` in order (LSB-first) into a wider value of type `ty`.
    Merge {
        parts: Vec<Var>,
        ty: Ty,
    },
    /// Broadcast a single-bit value across every bit position of type `ty`.
    Splat {
        src: Var,
        ty: Ty,
    },
    /// Arbitrary bit shuffle: assemble an output from individually selected bits.
    ///
    /// `result_bits[i] = (bit_idx, var)` — bit `i` of the output is taken
    /// from bit `bit_idx` of `var`.  Length equals the output bit-width.
    Shuffle {
        result_bits: Vec<(u8, Var)>,
        ty: Ty,
    },

    // ---- External access primitives ----------------------------------------

    /// Invoke a named pure oracle, producing a multi-output aggregate result.
    ///
    /// The result type is `IrType::Tuple(output_tys)`, pre-interned as
    /// `result_ty`.  Project individual outputs with [`OracleOutput`].
    ///
    /// **Ordering**: an `OracleCall` is pure and may be reordered or CSE'd
    /// freely.  It may only be DCE'd when every corresponding `OracleOutput`
    /// is also DCE'd.
    OracleCall {
        name: alloc::string::String,
        args: Vec<Var>,
        /// Return type of each output, in declaration order.  Non-empty.
        output_tys: Vec<Ty>,
        /// Pre-interned `TypeId` of `IrType::Tuple(output_tys)`.
        /// Stored at construction time so type inference never mutates the table.
        result_ty: Ty,
    },

    /// Project output `idx` from an [`OracleCall`] result var.
    ///
    /// `call` must be the SSA var produced by an `OracleCall` in the same block.
    /// `ty` must equal `oracle_call.output_tys[idx]`.
    OracleOutput {
        call: Var,
        idx: usize,
        ty: Ty,
    },

    /// Conditionally invoke a named impure action, producing a multi-output aggregate result.
    ///
    /// The result type is `IrType::Tuple(output_tys)`, pre-interned as `result_ty`.
    /// Output `i` is `action(args)[i]` when `guard != 0`, `fallbacks[i]` otherwise.
    /// Project individual outputs with [`ActionOutput`].
    ///
    /// **Ordering**: an `ActionCall` has side effects and must not be
    /// reordered, CSE'd, or DCE'd.  All `ActionOutput` projections from it
    /// are kept alive as long as the call itself is live.
    ActionCall {
        name: alloc::string::String,
        guard: Var,
        args: Vec<Var>,
        /// Fallback vars — one per output (typed as `output_tys[i]`).  Used
        /// when `guard = 0` and the action is not invoked.
        fallbacks: Vec<Var>,
        /// Return type of each output, in declaration order.  Non-empty.
        output_tys: Vec<Ty>,
        /// Pre-interned `TypeId` of `IrType::Tuple(output_tys)`.
        result_ty: Ty,
    },

    /// Project output `idx` from an [`ActionCall`] result var.
    ///
    /// `call` must be the SSA var produced by an `ActionCall` in the same block.
    /// `ty` must equal `action_call.output_tys[idx]`.
    ActionOutput {
        call: Var,
        idx: usize,
        ty: Ty,
    },

    /// Produce a fresh random value drawn uniformly from the type’s domain.
    ///
    /// `name` identifies the [`RngDecl`] in the enclosing [`IRBlocks::rngs`]
    /// that provides this source of randomness.  Each occurrence is an
    /// independent sample.  Optimisers must **not** deduplicate, CSE, or
    /// reorder `Rng` stmts.  An `Rng` may be DCE’d only when its output is
    /// demonstrably unused.
    Rng {
        /// Name of the declared RNG source (matches an [`RngDecl::name`]).
        name: alloc::string::String,
        ty: Ty,
    },
}

impl<Var: Ord, Ty, Stor> Stmt<Var, Var, Ty, Stor> {
    /// Convenience for the common `Addr = Var` case: map `Var` and `Addr`
    /// with a **single shared callback**, leaving `Ty` and `Stor` to their
    /// own callbacks.
    ///
    /// Avoids the borrow-checker conflict that arises when two closures both
    /// capture the same `&mut FnMut` to pass to [`Stmt::map`].
    pub fn map_var<Ctx, NV: Ord, NT, NS, E>(
        self,
        ctx: &mut Ctx,
        go: &mut impl FnMut(&mut Ctx, Var) -> Result<NV, E>,
        ty_fn: &mut impl FnMut(&mut Ctx, Ty) -> Result<NT, E>,
        stor_fn: &mut impl FnMut(&mut Ctx, Stor) -> Result<NS, E>,
    ) -> Result<Stmt<NV, NV, NT, NS>, E> {
        Ok(match self {
            Stmt::StorageRead { storage, ty, addr } => Stmt::StorageRead {
                storage: stor_fn(ctx, storage)?,
                ty: ty_fn(ctx, ty)?,
                addr: go(ctx, addr)?,
            },
            Stmt::StorageWrite { storage, src, ty, addr } => Stmt::StorageWrite {
                storage: stor_fn(ctx, storage)?,
                src: go(ctx, src)?,
                ty: ty_fn(ctx, ty)?,
                addr: go(ctx, addr)?,
            },
            Stmt::Const(c, ty) => Stmt::Const(c, ty_fn(ctx, ty)?),
            Stmt::Transmute { src, src_ty, dst_ty } => Stmt::Transmute {
                src: go(ctx, src)?,
                src_ty: ty_fn(ctx, src_ty)?,
                dst_ty: ty_fn(ctx, dst_ty)?,
            },
            Stmt::Poly { ty, coeffs, constant } => {
                let ty = ty_fn(ctx, ty)?;
                let coeffs = coeffs
                    .into_iter()
                    .map(|(mono, coeff)| {
                        let mono = mono.into_iter().map(|v| go(ctx, v)).collect::<Result<Vec<NV>, E>>()?;
                        Ok((mono, coeff))
                    })
                    .collect::<Result<BTreeMap<Vec<NV>, u8>, E>>()?;
                Stmt::Poly { ty, coeffs, constant }
            }
            Stmt::Rol { src, ty, n } => Stmt::Rol { src: go(ctx, src)?, ty: ty_fn(ctx, ty)?, n },
            Stmt::Ror { src, ty, n } => Stmt::Ror { src: go(ctx, src)?, ty: ty_fn(ctx, ty)?, n },
            Stmt::Merge { parts, ty } => Stmt::Merge {
                parts: parts.into_iter().map(|v| go(ctx, v)).collect::<Result<Vec<NV>, E>>()?,
                ty: ty_fn(ctx, ty)?,
            },
            Stmt::Splat { src, ty } => Stmt::Splat { src: go(ctx, src)?, ty: ty_fn(ctx, ty)? },
            Stmt::Shuffle { result_bits, ty } => Stmt::Shuffle {
                result_bits: result_bits
                    .into_iter()
                    .map(|(bit_idx, v)| Ok((bit_idx, go(ctx, v)?)))
                    .collect::<Result<Vec<(u8, NV)>, E>>()?,
                ty: ty_fn(ctx, ty)?,
            },
            Stmt::OracleCall { name, args, output_tys, result_ty } => Stmt::OracleCall {
                name,
                args: args.into_iter().map(|v| go(ctx, v)).collect::<Result<Vec<NV>, E>>()?,
                output_tys: output_tys.into_iter().map(|t| ty_fn(ctx, t)).collect::<Result<Vec<NT>, E>>()?,
                result_ty: ty_fn(ctx, result_ty)?,
            },
            Stmt::OracleOutput { call, idx, ty } => {
                Stmt::OracleOutput { call: go(ctx, call)?, idx, ty: ty_fn(ctx, ty)? }
            }
            Stmt::ActionCall { name, guard, args, fallbacks, output_tys, result_ty } => {
                Stmt::ActionCall {
                    name,
                    guard: go(ctx, guard)?,
                    args: args.into_iter().map(|v| go(ctx, v)).collect::<Result<Vec<NV>, E>>()?,
                    fallbacks: fallbacks.into_iter().map(|v| go(ctx, v)).collect::<Result<Vec<NV>, E>>()?,
                    output_tys: output_tys.into_iter().map(|t| ty_fn(ctx, t)).collect::<Result<Vec<NT>, E>>()?,
                    result_ty: ty_fn(ctx, result_ty)?,
                }
            }
            Stmt::ActionOutput { call, idx, ty } => {
                Stmt::ActionOutput { call: go(ctx, call)?, idx, ty: ty_fn(ctx, ty)? }
            }
            Stmt::Rng { name, ty } => Stmt::Rng { name, ty: ty_fn(ctx, ty)? },
        })
    }
}

impl<Var, Addr, Ty, Stor> Stmt<Var, Addr, Ty, Stor> {
    /// Map all four generic parameters simultaneously, potentially fallibly.
    ///
    /// `ctx` is passed by `&mut` to every callback so that all four callbacks
    /// can share mutable state (e.g., a `TypeTable` under construction or a
    /// `StorageAllocator`) without borrow conflicts.
    ///
    /// # Bounds
    /// `NV: Ord` is required because `Poly.coeffs` is a `BTreeMap<Vec<Var>, _>`
    /// and the new keys must remain ordered.
    ///
    /// # Non-generic fields
    /// `name` fields in `OracleCall`, `ActionCall`, and `Rng` are owned
    /// `String`s that are not parameterised by `Var`/`Addr`/`Ty`/`Stor`.
    /// They are moved into the result unchanged.
    pub fn map<Ctx, NV, NA, NT, NS, E>(
        self,
        ctx: &mut Ctx,
        mut var_fn: impl FnMut(&mut Ctx, Var) -> Result<NV, E>,
        mut addr_fn: impl FnMut(&mut Ctx, Addr) -> Result<NA, E>,
        mut ty_fn: impl FnMut(&mut Ctx, Ty) -> Result<NT, E>,
        mut stor_fn: impl FnMut(&mut Ctx, Stor) -> Result<NS, E>,
    ) -> Result<Stmt<NV, NA, NT, NS>, E>
    where
        NV: Ord,
    {
        Ok(match self {
            Stmt::StorageRead { storage, ty, addr } => Stmt::StorageRead {
                storage: stor_fn(ctx, storage)?,
                ty: ty_fn(ctx, ty)?,
                addr: addr_fn(ctx, addr)?,
            },
            Stmt::StorageWrite { storage, src, ty, addr } => Stmt::StorageWrite {
                storage: stor_fn(ctx, storage)?,
                src: var_fn(ctx, src)?,
                ty: ty_fn(ctx, ty)?,
                addr: addr_fn(ctx, addr)?,
            },
            Stmt::Const(c, ty) => Stmt::Const(c, ty_fn(ctx, ty)?),
            Stmt::Transmute { src, src_ty, dst_ty } => Stmt::Transmute {
                src: var_fn(ctx, src)?,
                src_ty: ty_fn(ctx, src_ty)?,
                dst_ty: ty_fn(ctx, dst_ty)?,
            },
            Stmt::Poly { ty, coeffs, constant } => {
                let ty = ty_fn(ctx, ty)?;
                let coeffs = coeffs
                    .into_iter()
                    .map(|(mono, coeff)| {
                        let mono = mono
                            .into_iter()
                            .map(|v| var_fn(ctx, v))
                            .collect::<Result<Vec<NV>, E>>()?;
                        Ok((mono, coeff))
                    })
                    .collect::<Result<BTreeMap<Vec<NV>, u8>, E>>()?;
                Stmt::Poly { ty, coeffs, constant }
            }
            Stmt::Rol { src, ty, n } => Stmt::Rol {
                src: var_fn(ctx, src)?,
                ty: ty_fn(ctx, ty)?,
                n,
            },
            Stmt::Ror { src, ty, n } => Stmt::Ror {
                src: var_fn(ctx, src)?,
                ty: ty_fn(ctx, ty)?,
                n,
            },
            Stmt::Merge { parts, ty } => Stmt::Merge {
                parts: parts.into_iter().map(|v| var_fn(ctx, v)).collect::<Result<Vec<NV>, E>>()?,
                ty: ty_fn(ctx, ty)?,
            },
            Stmt::Splat { src, ty } => Stmt::Splat {
                src: var_fn(ctx, src)?,
                ty: ty_fn(ctx, ty)?,
            },
            Stmt::Shuffle { result_bits, ty } => Stmt::Shuffle {
                result_bits: result_bits
                    .into_iter()
                    .map(|(bit_idx, v)| Ok((bit_idx, var_fn(ctx, v)?)))
                    .collect::<Result<Vec<(u8, NV)>, E>>()?,
                ty: ty_fn(ctx, ty)?,
            },
            Stmt::OracleCall { name, args, output_tys, result_ty } => Stmt::OracleCall {
                name,
                args: args.into_iter().map(|v| var_fn(ctx, v)).collect::<Result<Vec<NV>, E>>()?,
                output_tys: output_tys.into_iter().map(|t| ty_fn(ctx, t)).collect::<Result<Vec<NT>, E>>()?,
                result_ty: ty_fn(ctx, result_ty)?,
            },
            Stmt::OracleOutput { call, idx, ty } => Stmt::OracleOutput {
                call: var_fn(ctx, call)?,
                idx,
                ty: ty_fn(ctx, ty)?,
            },
            Stmt::ActionCall { name, guard, args, fallbacks, output_tys, result_ty } => {
                Stmt::ActionCall {
                    name,
                    guard: var_fn(ctx, guard)?,
                    args: args.into_iter().map(|v| var_fn(ctx, v)).collect::<Result<Vec<NV>, E>>()?,
                    fallbacks: fallbacks.into_iter().map(|v| var_fn(ctx, v)).collect::<Result<Vec<NV>, E>>()?,
                    output_tys: output_tys.into_iter().map(|t| ty_fn(ctx, t)).collect::<Result<Vec<NT>, E>>()?,
                    result_ty: ty_fn(ctx, result_ty)?,
                }
            }
            Stmt::ActionOutput { call, idx, ty } => Stmt::ActionOutput {
                call: var_fn(ctx, call)?,
                idx,
                ty: ty_fn(ctx, ty)?,
            },
            Stmt::Rng { name, ty } => Stmt::Rng { name, ty: ty_fn(ctx, ty)? },
        })
    }

    /// Borrow all generic parameters in place.
    ///
    /// Returns a `Stmt<&Var, &Addr, &Ty, &Stor>` whose fields are references
    /// into `self`.
    ///
    /// # Cost
    /// * Most variants: O(1) field borrows.
    /// * `Poly`: O(n log n) — the BTreeMap is rebuilt with `Vec<&Var>` keys.
    /// * `OracleCall`, `ActionCall`, `Rng`: the non-generic `name: String` is
    ///   **cloned** because it is not a generic parameter.
    ///
    /// # Bounds
    /// `Var: Ord` is required to rebuild the `Poly.coeffs` BTreeMap.
    pub fn as_ref(&self) -> Stmt<&Var, &Addr, &Ty, &Stor>
    where
        Var: Ord,
    {
        match self {
            Stmt::StorageRead { storage, ty, addr } => {
                Stmt::StorageRead { storage, ty, addr }
            }
            Stmt::StorageWrite { storage, src, ty, addr } => {
                Stmt::StorageWrite { storage, src, ty, addr }
            }
            Stmt::Const(c, ty) => Stmt::Const(*c, ty),
            Stmt::Transmute { src, src_ty, dst_ty } => {
                Stmt::Transmute { src, src_ty, dst_ty }
            }
            Stmt::Poly { ty, coeffs, constant } => {
                let coeffs = coeffs
                    .iter()
                    .map(|(mono, coeff)| (mono.iter().collect::<Vec<&Var>>(), *coeff))
                    .collect::<BTreeMap<Vec<&Var>, u8>>();
                Stmt::Poly { ty, coeffs, constant: *constant }
            }
            Stmt::Rol { src, ty, n } => Stmt::Rol { src, ty, n: *n },
            Stmt::Ror { src, ty, n } => Stmt::Ror { src, ty, n: *n },
            Stmt::Merge { parts, ty } => Stmt::Merge { parts: parts.iter().collect(), ty },
            Stmt::Splat { src, ty } => Stmt::Splat { src, ty },
            Stmt::Shuffle { result_bits, ty } => Stmt::Shuffle {
                result_bits: result_bits.iter().map(|(b, v)| (*b, v)).collect(),
                ty,
            },
            Stmt::OracleCall { name, args, output_tys, result_ty } => Stmt::OracleCall {
                name: name.clone(),
                args: args.iter().collect(),
                output_tys: output_tys.iter().collect(),
                result_ty,
            },
            Stmt::OracleOutput { call, idx, ty } => {
                Stmt::OracleOutput { call, idx: *idx, ty }
            }
            Stmt::ActionCall { name, guard, args, fallbacks, output_tys, result_ty } => {
                Stmt::ActionCall {
                    name: name.clone(),
                    guard,
                    args: args.iter().collect(),
                    fallbacks: fallbacks.iter().collect(),
                    output_tys: output_tys.iter().collect(),
                    result_ty,
                }
            }
            Stmt::ActionOutput { call, idx, ty } => {
                Stmt::ActionOutput { call, idx: *idx, ty }
            }
            Stmt::Rng { name, ty } => Stmt::Rng { name: name.clone(), ty },
        }
    }
}

// ============================================================================
// Generic substitution utilities
// ============================================================================

/// Merges a guest [`TypeTable`] into a host [`TypeTable`], producing a mapping
/// from guest [`TypeId`]s to their equivalent host [`TypeId`]s.
///
/// Types that are structurally identical to an existing host type share the
/// same [`TypeId`] (via [`TypeTable::intern`]).  New types are appended.
///
/// Reusable in any pass that combines programs from different modules.
pub struct TypeRemapper {
    /// `map[guest_id.0]` = the corresponding [`TypeId`] in the host table.
    pub map: alloc::vec::Vec<TypeId>,
}

impl TypeRemapper {
    /// Merge `guest` into `host` and return the resulting remapper.
    pub fn merge(host: &mut TypeTable, guest: &TypeTable) -> TypeRemapper {
        let n = guest.0.len();
        let mut map = alloc::vec![TypeId(0); n];
        let mut done = alloc::vec![false; n];
        for i in 0..n {
            Self::remap_one(i, guest, host, &mut map, &mut done);
        }
        TypeRemapper { map }
    }

    fn remap_one(
        idx: usize,
        guest: &TypeTable,
        host: &mut TypeTable,
        map: &mut alloc::vec::Vec<TypeId>,
        done: &mut alloc::vec::Vec<bool>,
    ) -> TypeId {
        if done[idx] {
            return map[idx];
        }
        done[idx] = true; // set before recursing (cycle guard)
        let remapped = match &guest.0[idx] {
            IrType::Primitive(p) => IrType::Primitive(*p),
            IrType::Vec(n, inner) => {
                let inner_host = Self::remap_one(inner.0 as usize, guest, host, map, done);
                IrType::Vec(*n, inner_host)
            }
            IrType::Tuple(parts) => {
                let parts_host: alloc::vec::Vec<TypeId> = parts
                    .iter()
                    .map(|p| Self::remap_one(p.0 as usize, guest, host, map, done))
                    .collect();
                IrType::Tuple(parts_host)
            }
            IrType::Block { params } => {
                let params_host: alloc::vec::Vec<TypeId> = params
                    .iter()
                    .map(|p| Self::remap_one(p.0 as usize, guest, host, map, done))
                    .collect();
                IrType::Block { params: params_host }
            }
            IrType::Func { params, results } => {
                let params_host: alloc::vec::Vec<TypeId> = params
                    .iter()
                    .map(|p| Self::remap_one(p.0 as usize, guest, host, map, done))
                    .collect();
                let results_host: alloc::vec::Vec<TypeId> = results
                    .iter()
                    .map(|r| Self::remap_one(r.0 as usize, guest, host, map, done))
                    .collect();
                IrType::Func { params: params_host, results: results_host }
            }
        };
        let host_id = host.intern(remapped);
        map[idx] = host_id;
        host_id
    }

    /// Remap a single guest [`TypeId`] to its host equivalent.
    #[inline]
    pub fn remap(&self, id: TypeId) -> TypeId {
        self.map[id.0 as usize]
    }

    /// Remap every [`TypeId`] field in a [`Stmt<V, A>`] in-place.
    ///
    /// Variable references (`V`, `A`) are left unchanged.
    pub fn remap_stmt_types<V: Clone, A: Clone>(&self, stmt: &mut Stmt<V, A>) {
        match stmt {
            Stmt::StorageRead { ty, .. } => {
                *ty = self.remap(*ty);
            }
            Stmt::StorageWrite { ty, .. } => {
                *ty = self.remap(*ty);
            }
            Stmt::Const(_, ty) => {
                *ty = self.remap(*ty);
            }
            Stmt::Transmute { src_ty, dst_ty, .. } => {
                *src_ty = self.remap(*src_ty);
                *dst_ty = self.remap(*dst_ty);
            }
            Stmt::Poly { ty, coeffs: _, constant: _ } => {
                *ty = self.remap(*ty);
            }
            Stmt::Rol { ty, .. } | Stmt::Ror { ty, .. } => {
                *ty = self.remap(*ty);
            }
            Stmt::Merge { ty, .. } => {
                *ty = self.remap(*ty);
            }
            Stmt::Splat { ty, .. } => {
                *ty = self.remap(*ty);
            }
            Stmt::Shuffle { ty, .. } => {
                *ty = self.remap(*ty);
            }
            Stmt::OracleCall { output_tys, result_ty, .. } => {
                for t in output_tys.iter_mut() {
                    *t = self.remap(*t);
                }
                *result_ty = self.remap(*result_ty);
            }
            Stmt::OracleOutput { ty, .. } => {
                *ty = self.remap(*ty);
            }
            Stmt::ActionCall { output_tys, result_ty, .. } => {
                for t in output_tys.iter_mut() {
                    *t = self.remap(*t);
                }
                *result_ty = self.remap(*result_ty);
            }
            Stmt::ActionOutput { ty, .. } => {
                *ty = self.remap(*ty);
            }
            Stmt::Rng { ty, .. } => {
                *ty = self.remap(*ty);
            }
        }
    }

    /// Remap [`TypeId`]s inside an [`OracleDecl`].
    pub fn remap_oracle_decl(&self, decl: &mut OracleDecl) {
        for t in decl.params.iter_mut() { *t = self.remap(*t); }
        for t in decl.results.iter_mut() { *t = self.remap(*t); }
    }

    /// Remap [`TypeId`]s inside an [`ActionDecl`].
    pub fn remap_action_decl(&self, decl: &mut ActionDecl) {
        for t in decl.params.iter_mut() { *t = self.remap(*t); }
        for t in decl.results.iter_mut() { *t = self.remap(*t); }
    }

    /// Remap the [`TypeId`] inside an [`RngDecl`].
    pub fn remap_rng_decl(&self, decl: &mut RngDecl) {
        decl.ty = self.remap(decl.ty);
    }

    /// Remap captured-input [`TypeId`]s inside an [`InstructionGroupDecl`].
    pub fn remap_instruction_group_decl(&self, decl: &mut InstructionGroupDecl) {
        for ty in &mut decl.params { *ty = self.remap(*ty); }
    }
}

/// Allocates fresh [`StorageId`]s above the range already in use.
///
/// Call [`StorageAllocator::new`] with `first_free` set to one above the
/// maximum [`StorageId`] observed in the program (clamped to ≥ 64 to avoid
/// the reserved protocol range).
pub struct StorageAllocator {
    pub next: u32,
}

impl StorageAllocator {
    /// Create an allocator starting at `first_free`.
    ///
    /// The caller is responsible for scanning the program to find the current
    /// maximum StorageId and passing `max + 1` here (clamped to ≥ 64).
    pub fn new(first_free: u32) -> Self {
        StorageAllocator { next: first_free }
    }

    /// Allocate the next fresh [`StorageId`].
    pub fn alloc(&mut self) -> StorageId {
        let id = StorageId(self.next);
        self.next += 1;
        id
    }
}

// ============================================================================
// Node: shared per-value metadata wrapper
// ============================================================================

/// The complete metadata carried by an IR node.
///
/// `Node` has exactly one metadata type parameter. Implementations use
/// associated types for metadata whose representation varies between IR users,
/// rather than adding another type parameter to `Node` for every metadata axis.
pub trait NodeMetadata: Clone {
    /// Source/debug provenance associated with this node.
    type Provenance: Clone;
    /// Per-value party/role annotation associated with this node.
    type Side: Clone;

    /// Borrow this node's provenance annotation.
    fn provenance(&self) -> &Self::Provenance;
    /// Borrow this node's side annotation.
    fn side(&self) -> &Self::Side;
}

/// A lexically ordered stack of static instruction-group instances.
///
/// The stack is private so additions/remapping go through explicit operations;
/// the outermost group is at index zero.
#[derive(Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct GroupMembership {
    stack: Vec<InstructionGroupId>,
}

impl GroupMembership {
    /// Construct an empty group stack.
    pub fn empty() -> Self { Self { stack: Vec::new() } }

    /// Construct a membership stack from already validated outer-to-inner IDs.
    pub fn new(stack: Vec<InstructionGroupId>) -> Self { Self { stack } }

    /// Borrow the outer-to-inner static instance IDs.
    pub fn stack(&self) -> &[InstructionGroupId] { &self.stack }

    /// Whether this node belongs to no instruction group.
    pub fn is_empty(&self) -> bool { self.stack.is_empty() }

    /// Fallibly remap every static instance ID.
    ///
    /// This remaps node metadata during cloning, inlining, or module-level ID
    /// renumbering. It intentionally does not touch captured SSA references in
    /// [`InstructionGroupInstance`] tables.
    pub fn map_ids<E>(
        self,
        mut f: impl FnMut(InstructionGroupId) -> Result<InstructionGroupId, E>,
    ) -> Result<Self, E> {
        Ok(Self { stack: self.stack.into_iter().map(&mut f).collect::<Result<Vec<_>, _>>()? })
    }
}

/// Policy for combining two instruction-group membership stacks.
///
/// A two-source transformation must state this policy rather than inheriting a
/// stack from an arbitrary source.
pub trait MapGroupMembership2 {
    /// Error returned when the two memberships cannot be combined.
    type Error;

    /// Produce the membership for the combined result.
    fn map_instruction_groups2(
        &self,
        left: GroupMembership,
        right: GroupMembership,
    ) -> Result<GroupMembership, Self::Error>;
}

/// Error returned by [`RequireEqualGroupMembership`] when source stacks differ.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub struct GroupMembershipMismatch;

/// Two-source group policy that preserves a stack only when both inputs have
/// exactly the same membership.
#[derive(Clone, Copy, Default, Debug)]
pub struct RequireEqualGroupMembership;

impl MapGroupMembership2 for RequireEqualGroupMembership {
    type Error = GroupMembershipMismatch;

    fn map_instruction_groups2(
        &self,
        left: GroupMembership,
        right: GroupMembership,
    ) -> Result<GroupMembership, Self::Error> {
        if left == right { Ok(left) } else { Err(GroupMembershipMismatch) }
    }
}

/// Function-backed instruction-group merge policy for an audited specialised
/// consumer, such as a group replacement lowering.
pub struct GroupMembershipMap2<F>(pub F);

impl<F, E> MapGroupMembership2 for GroupMembershipMap2<F>
where
    F: Fn(GroupMembership, GroupMembership) -> Result<GroupMembership, E>,
{
    type Error = E;

    fn map_instruction_groups2(
        &self,
        left: GroupMembership,
        right: GroupMembership,
    ) -> Result<GroupMembership, Self::Error> {
        (self.0)(left, right)
    }
}

/// The standard metadata used by Volar's IR pipeline.
///
/// Fields are private so a payload rewrite cannot accidentally omit a future
/// metadata axis. Use [`Node::derived`], [`Node::map_kind`], or the focused
/// accessors/mappers instead.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct StandardMetadata<P: Clone = ()> {
    provenance: P,
    side: Option<volar_side::SideId>,
    instruction_groups: GroupMembership,
}

impl<P: Clone> StandardMetadata<P> {
    /// Create standard metadata from explicit provenance and side annotations.
    pub fn new(provenance: P, side: Option<volar_side::SideId>) -> Self {
        Self { provenance, side, instruction_groups: GroupMembership::empty() }
    }

    /// Map provenance without changing the side annotation.
    pub fn map_provenance<Q: Clone, E>(
        self,
        f: impl FnOnce(P) -> Result<Q, E>,
    ) -> Result<StandardMetadata<Q>, E> {
        Ok(StandardMetadata {
            provenance: f(self.provenance)?,
            side: self.side,
            instruction_groups: self.instruction_groups,
        })
    }

    /// Replace instruction-group membership without changing other metadata.
    pub fn with_instruction_groups(mut self, groups: GroupMembership) -> Self {
        self.instruction_groups = groups;
        self
    }

    /// Fallibly map instruction-group membership without changing other metadata.
    pub fn map_instruction_groups<E>(
        mut self,
        f: impl FnOnce(GroupMembership) -> Result<GroupMembership, E>,
    ) -> Result<Self, E> {
        self.instruction_groups = f(self.instruction_groups)?;
        Ok(self)
    }

    /// Borrow this node's instruction-group membership.
    pub fn instruction_groups(&self) -> &GroupMembership { &self.instruction_groups }

    /// Replace the side annotation without changing provenance.
    pub fn with_side(mut self, side: Option<volar_side::SideId>) -> Self {
        self.side = side;
        self
    }

    /// Fallibly map the side annotation without changing provenance.
    pub fn map_side<E>(
        mut self,
        f: impl FnOnce(Option<volar_side::SideId>) -> Result<Option<volar_side::SideId>, E>,
    ) -> Result<Self, E> {
        self.side = f(self.side)?;
        Ok(self)
    }
}

impl<P: Clone> NodeMetadata for StandardMetadata<P> {
    type Provenance = P;
    type Side = Option<volar_side::SideId>;

    fn provenance(&self) -> &P { &self.provenance }
    fn side(&self) -> &Option<volar_side::SideId> { &self.side }
}

/// Error returned by [`Node::map2`].
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[non_exhaustive]
pub enum Map2Error<PayloadError, MetadataError> {
    /// The mapper for the two payloads failed.
    Payload(PayloadError),
    /// The mapper for the two metadata values failed.
    Metadata(MetadataError),
}

/// Policy for combining metadata from two source nodes.
///
/// There is deliberately no implicit "use the left metadata" behaviour:
/// callers combining two values must name a policy.
pub trait MapMetadata2<L: NodeMetadata, R: NodeMetadata> {
    /// Metadata carried by the merged node.
    type Output: NodeMetadata;
    /// Error reported when metadata cannot be combined.
    type Error;

    /// Combine complete left and right metadata values.
    fn map_metadata2(&self, left: L, right: R) -> Result<Self::Output, Self::Error>;
}

/// Policy for combining the standard side metadata axis.
pub trait MapSide2 {
    /// Error reported by side combination.
    type Error;

    /// Combine two side annotations.
    fn map_side2(
        &self,
        left: Option<volar_side::SideId>,
        right: Option<volar_side::SideId>,
    ) -> Result<Option<volar_side::SideId>, Self::Error>;
}

/// The normal two-source side policy: retain the common side, otherwise none.
#[derive(Clone, Copy, Default, Debug)]
pub struct DefaultSideMap2;

impl MapSide2 for DefaultSideMap2 {
    type Error = core::convert::Infallible;

    fn map_side2(
        &self,
        left: Option<volar_side::SideId>,
        right: Option<volar_side::SideId>,
    ) -> Result<Option<volar_side::SideId>, Self::Error> {
        Ok(volar_side::propagate(&[left, right]))
    }
}

/// Error reported by [`StandardMetadataMap2`].
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[non_exhaustive]
pub enum StandardMetadataMap2Error<SideError, GroupError> {
    /// The configured side policy rejected the pair.
    Side(SideError),
    /// The configured instruction-group policy rejected the pair.
    InstructionGroups(GroupError),
}

/// Standard metadata merge policy.
///
/// Provenance is combined by a [`volar_provenance::DualProvenanceHandler`];
/// side uses [`DefaultSideMap2`] and group membership requires exact equality
/// unless specialised policies are supplied.
pub struct StandardMetadataMap2<
    Prov,
    Side = DefaultSideMap2,
    Groups = RequireEqualGroupMembership,
> {
    /// Provenance merge policy.
    pub provenance: Prov,
    /// Side merge policy.
    pub side: Side,
    /// Instruction-group membership merge policy.
    pub instruction_groups: Groups,
}

impl<Prov> StandardMetadataMap2<Prov, DefaultSideMap2, RequireEqualGroupMembership> {
    /// Construct the standard policy with default side propagation and exact
    /// instruction-group stack preservation.
    pub fn new(provenance: Prov) -> Self {
        Self {
            provenance,
            side: DefaultSideMap2,
            instruction_groups: RequireEqualGroupMembership,
        }
    }
}

impl<P1, P2, Prov, Side, Groups> MapMetadata2<StandardMetadata<P1>, StandardMetadata<P2>>
    for StandardMetadataMap2<Prov, Side, Groups>
where
    P1: Clone,
    P2: Clone,
    Prov: volar_provenance::DualProvenanceHandler<P1, P2>,
    Side: MapSide2,
    Groups: MapGroupMembership2,
{
    type Output = StandardMetadata<Prov::Output>;
    type Error = StandardMetadataMap2Error<Side::Error, Groups::Error>;

    fn map_metadata2(
        &self,
        left: StandardMetadata<P1>,
        right: StandardMetadata<P2>,
    ) -> Result<Self::Output, Self::Error> {
        let side = self.side.map_side2(left.side, right.side)
            .map_err(StandardMetadataMap2Error::Side)?;
        let instruction_groups = self
            .instruction_groups
            .map_instruction_groups2(left.instruction_groups, right.instruction_groups)
            .map_err(StandardMetadataMap2Error::InstructionGroups)?;
        Ok(StandardMetadata {
            provenance: self.provenance.merge(&left.provenance, &right.provenance),
            side,
            instruction_groups,
        })
    }
}

impl<Prov, Side, Groups> StandardMetadataMap2<Prov, Side, Groups> {
    /// Map metadata originating solely from the left source.
    ///
    /// The side annotation is copied: a one-source derivation is not a side
    /// merge and therefore does not invoke the two-source side policy.
    pub fn map_left<P1: Clone, P2: Clone>(
        &self,
        left: StandardMetadata<P1>,
    ) -> Result<StandardMetadata<Prov::Output>, core::convert::Infallible>
    where
        Prov: volar_provenance::DualProvenanceHandler<P1, P2>,
    {
        Ok(StandardMetadata::new(
            self.provenance.map_left(&left.provenance),
            left.side,
        ).with_instruction_groups(left.instruction_groups))
    }

    /// Map metadata originating solely from the right source.
    ///
    /// The side annotation is copied: a one-source derivation is not a side
    /// merge and therefore does not invoke the two-source side policy.
    pub fn map_right<P1: Clone, P2: Clone>(
        &self,
        right: StandardMetadata<P2>,
    ) -> Result<StandardMetadata<Prov::Output>, core::convert::Infallible>
    where
        Prov: volar_provenance::DualProvenanceHandler<P1, P2>,
    {
        Ok(StandardMetadata::new(
            self.provenance.map_right(&right.provenance),
            right.side,
        ).with_instruction_groups(right.instruction_groups))
    }
}

/// A node wrapping an IR/AST payload `T` and complete metadata `M`.
///
/// The default metadata type carries ordinary Volar provenance and side
/// annotations. Derived nodes copy their complete metadata; transformations
/// that merge two sources use [`Node::map2`] with a [`MapMetadata2`] policy.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct Node<T, M: NodeMetadata = StandardMetadata<()>> {
    pub kind: T,
    metadata: M,
}

impl<T, M: NodeMetadata> Node<T, M> {
    /// Construct a node with explicit complete metadata.
    pub fn with_metadata(kind: T, metadata: M) -> Self {
        Node { kind, metadata }
    }

    /// Derive a node with a new payload and an exact clone of this node's
    /// complete metadata.
    pub fn derived<U>(&self, kind: U) -> Node<U, M> {
        Node { kind, metadata: self.metadata.clone() }
    }

    /// Fallibly map the payload while preserving complete metadata.
    pub fn map_kind<U, E>(self, f: impl FnOnce(T) -> Result<U, E>) -> Result<Node<U, M>, E> {
        Ok(Node { kind: f(self.kind)?, metadata: self.metadata })
    }

    /// Fallibly map complete metadata while preserving the payload.
    pub fn map_metadata<N: NodeMetadata, E>(
        self,
        f: impl FnOnce(M) -> Result<N, E>,
    ) -> Result<Node<T, N>, E> {
        Ok(Node { kind: self.kind, metadata: f(self.metadata)? })
    }

    /// Fallibly combine two payloads and their metadata through explicit
    /// policies. No metadata axis is inherited implicitly from either input.
    pub fn map2<U, N, V, PE, MM>(
        self,
        other: Node<U, N>,
        map_kind: impl FnOnce(T, U) -> Result<V, PE>,
        map_metadata: &MM,
    ) -> Result<Node<V, MM::Output>, Map2Error<PE, MM::Error>>
    where
        N: NodeMetadata,
        MM: MapMetadata2<M, N>,
    {
        let kind = map_kind(self.kind, other.kind).map_err(Map2Error::Payload)?;
        let metadata = map_metadata
            .map_metadata2(self.metadata, other.metadata)
            .map_err(Map2Error::Metadata)?;
        Ok(Node { kind, metadata })
    }

    /// Borrow this node's complete metadata.
    pub fn metadata(&self) -> &M { &self.metadata }
}

impl<T, P: Clone> Node<T, StandardMetadata<P>> {
    /// Construct a standard-metadata node from provenance and side annotations.
    pub fn new(kind: T, prov: P, side: Option<volar_side::SideId>) -> Self {
        Node::with_metadata(kind, StandardMetadata::new(prov, side))
    }

    /// Borrow this node's provenance annotation.
    pub fn provenance(&self) -> &P { &self.metadata.provenance }

    /// Return this node's side annotation.
    pub fn side(&self) -> Option<volar_side::SideId> { self.metadata.side }

    /// Borrow this node's instruction-group membership.
    pub fn instruction_groups(&self) -> &GroupMembership { self.metadata.instruction_groups() }

    /// Replace this node's instruction-group membership while preserving
    /// provenance and side annotations.
    pub fn with_instruction_groups(self, groups: GroupMembership) -> Self {
        Node::with_metadata(self.kind, self.metadata.with_instruction_groups(groups))
    }

    /// Fallibly map this node's instruction-group membership while preserving
    /// provenance and side annotations.
    pub fn map_instruction_groups<E>(
        self,
        f: impl FnOnce(GroupMembership) -> Result<GroupMembership, E>,
    ) -> Result<Self, E> {
        self.map_metadata(|metadata| metadata.map_instruction_groups(f))
    }

    /// Fallibly map this node's provenance while preserving all other metadata.
    pub fn map_prov<Q: Clone, E>(
        self,
        f: impl FnOnce(P) -> Result<Q, E>,
    ) -> Result<Node<T, StandardMetadata<Q>>, E> {
        self.map_metadata(|metadata| metadata.map_provenance(f))
    }
}

/// Implemented by a tree-shaped `Kind` payload that itself embeds provenance
/// in nested [`Node`]s.
pub trait MapKind<P: Clone, Q: Clone> {
    /// The same `Kind` shape with every nested `P` replaced by `Q`.
    type Output;

    /// Recurse into `self`, replacing every nested `P` via `f`.
    fn map_kind(self, f: &impl Fn(P) -> Q) -> Self::Output;
}

impl<T, P: Clone> Node<T, StandardMetadata<P>> {
    /// Map provenance through a tree-shaped payload that itself embeds
    /// provenance in nested nodes, preserving all other metadata.
    ///
    /// This compatibility adapter preserves the historic tree-IR mapping
    /// contract. New fallible transformations should use the container-level
    /// [`Node::map_kind`] and [`Node::map_metadata`] APIs.
    pub fn map_kind_prov<Q: Clone>(
        self,
        f: &impl Fn(P) -> Q,
    ) -> Node<T::Output, StandardMetadata<Q>>
    where
        T: MapKind<P, Q>,
    {
        let metadata = self.metadata.map_provenance(|p| Ok::<_, core::convert::Infallible>(f(p)))
            .expect("infallible tree provenance mapping");
        let kind = self.kind.map_kind(f);
        Node { kind, metadata }
    }
}

#[cfg(test)]
mod metadata_tests {
    extern crate std;

    use alloc::{borrow::ToOwned, vec};
    use super::*;
    use volar_provenance::MergePair;

    #[test]
    fn derived_and_map_kind_preserve_complete_standard_metadata() {
        let node = Node::new(3u32, "source", Some(volar_side::SideId(4)));
        let derived = node.derived("derived");
        assert_eq!(derived.kind, "derived");
        assert_eq!(derived.provenance(), &"source");
        assert_eq!(derived.side(), Some(volar_side::SideId(4)));

        let mapped = node
            .map_kind(|kind| Ok::<_, ()>(kind + 1))
            .expect("infallible payload mapping");
        assert_eq!(mapped.kind, 4);
        assert_eq!(mapped.provenance(), &"source");
        assert_eq!(mapped.side(), Some(volar_side::SideId(4)));
    }

    #[test]
    fn map2_uses_dual_provenance_and_default_side_policy() {
        let left = Node::new(2u32, "left", Some(volar_side::SideId(9)));
        let right = Node::new(3u32, "right", Some(volar_side::SideId(9)));
        let mapper = StandardMetadataMap2::new(MergePair(
            |left: &&str| (*left).to_owned(),
            |right: &&str| (*right).to_owned(),
            |left: &&str, right: &&str| std::format!("{left}+{right}"),
        ));

        let mapped = left
            .map2(right, |left, right| Ok::<_, ()>(left + right), &mapper)
            .expect("infallible metadata mapping");
        assert_eq!(mapped.kind, 5);
        assert_eq!(mapped.provenance(), "left+right");
        assert_eq!(mapped.side(), Some(volar_side::SideId(9)));
    }

    struct RejectSide;

    impl MapSide2 for RejectSide {
        type Error = u8;

        fn map_side2(
            &self,
            _: Option<volar_side::SideId>,
            _: Option<volar_side::SideId>,
        ) -> Result<Option<volar_side::SideId>, Self::Error> {
            Err(7)
        }
    }

    #[test]
    fn map2_distinguishes_payload_and_metadata_errors() {
        let left = Node::new(1u32, (), None);
        let right = Node::new(2u32, (), None);
        let mapper = StandardMetadataMap2 {
            provenance: MergePair(|_: &()| (), |_: &()| (), |_: &(), _: &()| ()),
            side: RejectSide,
            instruction_groups: RequireEqualGroupMembership,
        };

        assert!(matches!(
            left.clone().map2(right.clone(), |_, _| Err::<u32, _>(5u16), &mapper),
            Err(Map2Error::Payload(5)),
        ));
        assert!(matches!(
            left.map2(right, |a, b| Ok::<_, u16>(a + b), &mapper),
            Err(Map2Error::Metadata(StandardMetadataMap2Error::Side(7))),
        ));
    }

    #[test]
    fn group_membership_is_preserved_and_requires_an_explicit_valid_merge() {
        let groups = GroupMembership::new(vec![InstructionGroupId(2), InstructionGroupId(5)]);
        let node = Node::new(3u32, "source", None).with_instruction_groups(groups.clone());
        assert_eq!(node.derived(4u32).instruction_groups(), &groups);
        assert_eq!(
            node.clone()
                .map_kind(|kind| Ok::<_, ()>(kind + 1))
                .expect("infallible payload mapping")
                .instruction_groups(),
            &groups,
        );

        let mapper = StandardMetadataMap2::new(MergePair(
            |left: &&str| (*left).to_owned(),
            |right: &&str| (*right).to_owned(),
            |left: &&str, right: &&str| std::format!("{left}+{right}"),
        ));
        assert!(node.clone().map2(
            Node::new(4u32, "other", None),
            |left, right| Ok::<_, ()>(left + right),
            &mapper,
        ).is_err());

        let mapped = node.map2(
            Node::new(4u32, "other", None).with_instruction_groups(groups.clone()),
            |left, right| Ok::<_, ()>(left + right),
            &mapper,
        ).expect("equal group membership");
        assert_eq!(mapped.instruction_groups(), &groups);
    }

    #[test]
    fn group_membership_id_mapping_is_fallible() {
        let groups = GroupMembership::new(vec![InstructionGroupId(2), InstructionGroupId(5)]);
        let remapped = groups.clone().map_ids(|id| Ok::<_, ()>(InstructionGroupId(id.0 + 10)))
            .expect("infallible ID remapping");
        assert_eq!(remapped.stack(), &[InstructionGroupId(12), InstructionGroupId(15)]);
        assert!(groups.map_ids(|id| {
            if id == InstructionGroupId(5) { Err(()) } else { Ok(id) }
        }).is_err());
    }
}
