// @reliability: normal
//! Struct registry: maps `StructKind` names to `StructId`s and field info.
//!
//! Built from a (monomorphized) `IrModule` by calling `define_struct` on the
//! target for each concrete struct. Provides field-index lookup for `Field` and
//! `StructExpr` lowering.

use std::collections::BTreeMap;
use volar_compiler::ir::{ArrayLength, IrModule, IrStruct, IrType, PrimitiveType, StructKind};
use volar_ir_common::Type as NativeType;
use volar_lir::{FieldDef, LirTarget, LirType, StructDef, StructId};

// ============================================================================
// StructRegistry
// ============================================================================

/// Lookup table from a struct's kind-name to its registered `StructId` and
/// field metadata.
pub struct StructRegistry {
    /// kind name (e.g., "Eval", "Garble") → entry
    by_name: BTreeMap<String, StructEntry>,
    /// kind name → native Volar type, for structs annotated `@volar-native:`.
    /// These structs are **not** registered as LIR structs; instead they map
    /// to `LirType::Native(t)` in all type-conversion contexts.
    native_types: BTreeMap<String, NativeType>,
}

struct StructEntry {
    pub id: StructId,
    /// Field names in declaration order.
    pub field_names: Vec<String>,
    /// Field LirTypes in declaration order.
    pub field_types: Vec<LirType>,
}

impl StructRegistry {
    fn new() -> Self {
        StructRegistry { by_name: BTreeMap::new(), native_types: BTreeMap::new() }
    }

    /// An empty registry (no structs registered). Used when no struct types are needed.
    pub fn empty() -> Self {
        Self::new()
    }

    /// Return `true` if `kind` was annotated `@volar-native:`.
    pub fn is_native(&self, kind: &StructKind) -> bool {
        self.native_types.contains_key(&kind_name(kind))
    }

    /// Return the `NativeType` for a native-annotated struct, if any.
    pub fn native_type_for(&self, kind: &StructKind) -> Option<NativeType> {
        self.native_types.get(&kind_name(kind)).copied()
    }

    /// Look up the `StructId` for a struct by its kind name.
    pub fn id_for(&self, kind: &StructKind) -> Option<StructId> {
        self.by_name.get(&kind_name(kind)).map(|e| e.id)
    }

    /// Return the 0-based index of `field` in the given struct, or panic.
    pub fn field_index(&self, id: StructId, field: &str) -> usize {
        for entry in self.by_name.values() {
            if entry.id == id {
                return entry
                    .field_names
                    .iter()
                    .position(|n| n == field)
                    .unwrap_or_else(|| {
                        panic!("struct S{id} has no field '{field}'")
                    });
            }
        }
        panic!("StructId {id} not in registry")
    }

    /// Return the field names for a struct in declaration order.
    pub fn field_names(&self, id: StructId) -> &[String] {
        for entry in self.by_name.values() {
            if entry.id == id {
                return &entry.field_names;
            }
        }
        panic!("StructId {id} not in registry")
    }

    /// Return the LirType of a field by struct id and field index.
    pub fn field_type(&self, id: StructId, field_idx: usize) -> &LirType {
        for entry in self.by_name.values() {
            if entry.id == id {
                return &entry.field_types[field_idx];
            }
        }
        panic!("StructId {id} not in registry")
    }

    /// Resolve an IrType to a LirType using this registry.
    /// Requires that all referenced structs are already in the registry.
    pub fn ir_type_to_lir(&self, ty: &IrType) -> LirType {
        ir_type_to_lir_inner(ty, self)
    }
}

// ============================================================================
// Builder
// ============================================================================

/// Build a `StructRegistry` from a (monomorphized) `IrModule`.
///
/// For each struct in `module.structs`:
/// 1. Maps its field types to `LirType`.
/// 2. Calls `target.define_struct(...)`.
/// 3. Records the mapping.
///
/// Structs are registered in the order they appear in `module.structs`.
/// The caller must ensure the ordering is dependency-safe (fields of struct S
/// must not reference struct T unless T appears earlier in the list).
pub fn build_struct_registry<T: LirTarget>(module: &IrModule, target: &mut T) -> StructRegistry {
    let mut registry = StructRegistry::new();

    for ir_struct in &module.structs {
        // Skip GenericArray — it maps to LirType::Arr directly, not a LIR struct.
        if ir_struct.kind == StructKind::GenericArray {
            continue;
        }

        let name = kind_name(&ir_struct.kind);

        // Structs annotated `@volar-native:` are emitted as a single native
        // field-element value.  Record them but do NOT register as LIR structs.
        if let Some(native_ty) = ir_struct.native_volar_type {
            registry.native_types.insert(name, native_ty);
            continue;
        }

        // Map field types to LirType using the partially-built registry.
        let lir_fields: Vec<(String, LirType)> = ir_struct
            .fields
            .iter()
            .map(|f| {
                let lir_ty = registry.ir_type_to_lir(&f.ty);
                (f.name.clone(), lir_ty)
            })
            .collect();

        let struct_def = StructDef {
            name: name.clone(),
            fields: lir_fields
                .iter()
                .map(|(fname, fty)| FieldDef { name: fname.clone(), ty: fty.clone() })
                .collect(),
        };

        let id = target.define_struct(struct_def);
        let field_names = lir_fields.iter().map(|(n, _)| n.clone()).collect();
        let field_types = lir_fields.into_iter().map(|(_, t)| t).collect();

        registry.by_name.insert(name, StructEntry { id, field_names, field_types });
    }

    registry
}

// ============================================================================
// Type conversion
// ============================================================================

/// Convert an `IrType` to a `LirType`, using the (possibly partial) registry
/// for struct lookup.
fn ir_type_to_lir_inner(ty: &IrType, registry: &StructRegistry) -> LirType {
    match ty {
        IrType::Primitive(p) => primitive_to_lir(*p),

        IrType::Unit => LirType::Bool, // unit → zero-bit placeholder

        IrType::Array { elem, len, .. } => {
            let n = match len {
                ArrayLength::Const(n) => *n,
                ArrayLength::TypeNum(tn) => tn.to_usize(),
                ArrayLength::TypeParam(name) => {
                    panic!("unsubstituted TypeParam length '{name}' — run monomorphize_module first")
                }
                ArrayLength::Projection { .. } => {
                    unimplemented!("Projection array length in LIR lowering")
                }
            };
            LirType::Arr(Box::new(ir_type_to_lir_inner(elem, registry)), n)
        }

        IrType::Struct { kind, .. } => {
            // Native-annotated structs map to LirType::Native instead of LirType::Struct.
            if let Some(native_ty) = registry.native_types.get(&kind_name(kind)).copied() {
                return LirType::Native(native_ty);
            }
            let id = registry.id_for(kind).unwrap_or_else(|| {
                panic!("struct '{:?}' not in registry — was define_struct called?", kind)
            });
            LirType::Struct(id)
        }

        IrType::Reference { elem, .. } => {
            // References are transparent in LIR (value semantics).
            ir_type_to_lir_inner(elem, registry)
        }

        IrType::TypeParam(name) => {
            panic!("unsubstituted TypeParam '{name}' — run monomorphize_module first")
        }

        other => {
            unimplemented!("ir_type_to_lir: unsupported type {:?}", other)
        }
    }
}

pub fn primitive_to_lir(p: PrimitiveType) -> LirType {
    match p {
        PrimitiveType::Bool | PrimitiveType::Bit => LirType::Bool,
        PrimitiveType::U8 | PrimitiveType::Galois | PrimitiveType::BitsInBytes => LirType::U8,
        PrimitiveType::U32 => LirType::U32,
        PrimitiveType::U64
        | PrimitiveType::Galois64
        | PrimitiveType::BitsInBytes64
        | PrimitiveType::Usize => LirType::U64,
        PrimitiveType::I128 | PrimitiveType::U128 => LirType::I64,
        // 128-bit and 256-bit fields don't have direct LIR lowerings yet;
        // map them to the closest available integer type for now.
        PrimitiveType::Galois128 => LirType::I64, // placeholder
        PrimitiveType::Galois256 => LirType::I64, // placeholder
    }
}

pub fn kind_name(kind: &StructKind) -> String {
    match kind {
        StructKind::Custom(name) => name.clone(),
        StructKind::GenericArray => "Array".to_string(),
    }
}

/// Determine the concrete array length from a (monomorphized) IrStruct,
/// used to build struct names that include the instantiation size.
pub fn struct_c_name(ir_struct: &IrStruct) -> String {
    // For now just use the kind name; extend to include size suffix if needed.
    kind_name(&ir_struct.kind)
}

// ============================================================================
// Flattening utilities
// ============================================================================

impl StructRegistry {
    /// Return the LirTypes of all fields in declaration order.
    pub fn field_types(&self, id: StructId) -> &[LirType] {
        for entry in self.by_name.values() {
            if entry.id == id {
                return &entry.field_types;
            }
        }
        panic!("StructId {id} not in registry")
    }
}

/// Number of scalars needed to represent `ty` when fully flattened.
///
/// - Scalar types → 1
/// - `Arr(elem, n)` → n × flatten_count(elem)
/// - `Struct(id)` → sum of flatten_count for each field
pub fn flatten_count(ty: &LirType, registry: &StructRegistry) -> usize {
    match ty {
        LirType::Arr(elem, n) => n * flatten_count(elem, registry),
        LirType::Struct(id) => registry
            .field_types(*id)
            .iter()
            .map(|ft| flatten_count(ft, registry))
            .sum(),
        _ => 1,
    }
}

/// Ordered list of scalar `LirType`s that make up the flattened representation of `ty`.
///
/// E.g. `Struct(Eval<16>)` where `Eval { base: [u8; 16] }` → `[U8; 16]`.
pub fn flatten_scalar_types(ty: &LirType, registry: &StructRegistry) -> Vec<LirType> {
    match ty {
        LirType::Arr(elem, n) => {
            let elem_scalars = flatten_scalar_types(elem, registry);
            elem_scalars.iter().cloned().cycle().take(elem_scalars.len() * n).collect()
        }
        LirType::Struct(id) => registry
            .field_types(*id)
            .iter()
            .flat_map(|ft| flatten_scalar_types(ft, registry))
            .collect(),
        scalar => vec![scalar.clone()],
    }
}

/// Scalar offset (in elements) of `field_idx` within a flattened struct.
///
/// This is the number of scalars contributed by fields `0..field_idx`.
pub fn struct_field_scalar_offset(
    registry: &StructRegistry,
    id: StructId,
    field_idx: usize,
) -> usize {
    registry
        .field_types(id)
        .iter()
        .take(field_idx)
        .map(|ft| flatten_count(ft, registry))
        .sum()
}

/// Number of scalars contributed by `field_idx` within a flattened struct.
pub fn struct_field_scalar_width(
    registry: &StructRegistry,
    id: StructId,
    field_idx: usize,
) -> usize {
    flatten_count(&registry.field_types(id)[field_idx], registry)
}
