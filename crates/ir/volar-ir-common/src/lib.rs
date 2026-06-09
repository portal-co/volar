#![no_std]
// @reliability: normal
// @ai: assisted

extern crate alloc;

use alloc::{collections::btree_map::BTreeMap, vec::Vec};

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
