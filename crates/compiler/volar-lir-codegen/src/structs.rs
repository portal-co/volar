// @reliability: normal
// @ai: assisted
//! Struct registry: maps `StructKind` names to `StructId`s and field info.
//!
//! Built from a (monomorphized) `IrModule` by calling `define_struct` on the
//! target for each concrete struct. Provides field-index lookup for `Field` and
//! `StructExpr` lowering.

use std::collections::BTreeMap;
use volar_compiler::ir::{
    ArrayKind, ArrayLength, IrEnum, IrEnumVariantData, IrFunction, IrModule, IrStruct, IrType,
    PrimitiveType, StructKind,
};
use volar_ir_common::Type as NativeType;
use volar_lir::{FieldDef, LirTarget, LirType, StructDef, StructId};
use crate::mono::{MonoEnv, mono_type};

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
    /// When `true`, unknown struct types are treated as opaque (mapped to
    /// `LirType::U64` placeholder) rather than panicking.  Useful for backends
    /// (like C) that only need circuit-level code and can tolerate opaque
    /// external types in function signatures.
    pub lenient: bool,
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
        StructRegistry { by_name: BTreeMap::new(), native_types: BTreeMap::new(), lenient: false }
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

    /// Resolve an IrType to a LirType, applying `env` substitutions first.
    /// Requires that all referenced structs are already in the registry.
    pub fn ir_type_to_lir(&self, ty: &IrType, env: &MonoEnv) -> LirType {
        ir_type_to_lir_inner(&mono_type(ty, env), self)
    }

    /// Look up a struct by name string (used for synthetic tuple structs).
    pub fn id_for_name(&self, name: &str) -> Option<StructId> {
        self.by_name.get(name).map(|e| e.id)
    }

    /// Register a synthetic struct (e.g. for tuple types).  The caller is
    /// responsible for having already called `target.define_struct` to obtain
    /// the `StructId`.
    pub fn register_synthetic(
        &mut self,
        name: String,
        id: StructId,
        field_names: Vec<String>,
        field_types: Vec<LirType>,
    ) {
        self.by_name.insert(name, StructEntry { id, field_names, field_types });
    }
}

// ============================================================================
// Tuple struct registration
// ============================================================================

/// Generate a canonical name for a tuple type based on its element LirTypes.
fn tuple_struct_name(elem_lir_tys: &[LirType]) -> String {
    let mut name = String::from("__Tuple");
    for ty in elem_lir_tys {
        name.push('_');
        name.push_str(&lir_type_tag(ty));
    }
    name
}

/// Short tag for a LirType, used to build canonical tuple struct names.
fn lir_type_tag(ty: &LirType) -> String {
    match ty {
        LirType::Bool => "b".into(),
        LirType::U8 => "u8".into(),
        LirType::U32 => "u32".into(),
        LirType::U64 => "u64".into(),
        LirType::I64 => "i64".into(),
        LirType::Arr(elem, n) => format!("a{}x{}", lir_type_tag(elem), n),
        LirType::Struct(id) => format!("s{}", id),
        LirType::Ptr(elem) => format!("p{}", lir_type_tag(elem)),
        LirType::Native(n) => format!("n{}", *n as u8),
        _ => "x".into(),
    }
}

/// Recursively scan an `IrType` for tuple types and register them as
/// synthetic structs in the registry.  Must be called before `ir_type_to_lir`
/// encounters any tuple types.
pub fn register_tuples_in_type<T: LirTarget>(
    ty: &IrType,
    registry: &mut StructRegistry,
    target: &mut T,
    env: &MonoEnv,
) {
    let ty = &mono_type(ty, env);
    match ty {
        IrType::Tuple(elems) if !elems.is_empty() => {
            // Recurse into elements first (handles nested tuples).
            for elem in elems {
                register_tuples_in_type(elem, registry, target, env);
            }
            // Convert elements to LIR and check if already registered.
            let lir_elems: Vec<LirType> = elems.iter()
                .map(|e| ir_type_to_lir_inner(&mono_type(e, env), registry))
                .collect();
            let name = tuple_struct_name(&lir_elems);
            if registry.id_for_name(&name).is_some() {
                return; // already registered
            }
            // Register as a synthetic struct with fields "0", "1", ...
            let field_defs: Vec<FieldDef> = lir_elems.iter().enumerate()
                .map(|(i, t)| FieldDef { name: format!("{i}"), ty: t.clone() })
                .collect();
            let id = target.define_struct(StructDef {
                name: name.clone(),
                fields: field_defs,
            });
            let field_names = (0..lir_elems.len()).map(|i| format!("{i}")).collect();
            registry.register_synthetic(name, id, field_names, lir_elems);
        }
        IrType::Array { elem, .. } | IrType::Reference { elem, .. } => {
            register_tuples_in_type(elem, registry, target, env);
        }
        _ => {}
    }
}

// ============================================================================
// Builder
// ============================================================================

/// Build a `StructRegistry` from an `IrModule`, applying `env` substitutions
/// to field types on the fly.
///
/// For each struct in `module.structs`:
/// 1. Maps its field types to `LirType` (with mono substitution via `env`).
/// 2. Calls `target.define_struct(...)`.
/// 3. Records the mapping.
///
/// Structs are registered in the order they appear in `module.structs`.
/// The caller must ensure the ordering is dependency-safe (fields of struct S
/// must not reference struct T unless T appears earlier in the list).
pub fn build_struct_registry<T: LirTarget>(module: &IrModule<IrFunction>, target: &mut T, env: &MonoEnv) -> StructRegistry {
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

        // Map field types to LirType using the partially-built registry,
        // applying MonoEnv substitutions on the fly.
        let lir_fields: Vec<(String, LirType)> = ir_struct
            .fields
            .iter()
            .map(|f| {
                let lir_ty = registry.ir_type_to_lir(&f.ty, env);
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
                    panic!("unsubstituted TypeParam length '{name}' — add it to MonoEnv")
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
            match registry.id_for(kind) {
                Some(id) => LirType::Struct(id),
                None if registry.lenient => {
                    // Opaque external type — use U64 as a placeholder.
                    LirType::U64
                }
                None => {
                    panic!("struct '{:?}' not in registry — was define_struct called?", kind)
                }
            }
        }

        IrType::Reference { elem, .. } => {
            // References to slices become pointers — slices are dynamically
            // sized, so they can't be flattened to a fixed number of scalars.
            if let IrType::Array { kind: ArrayKind::Slice, elem: inner_elem, .. } = elem.as_ref() {
                LirType::Ptr(Box::new(ir_type_to_lir_inner(inner_elem, registry)))
            } else {
                // Non-slice references are transparent in LIR (value semantics).
                ir_type_to_lir_inner(elem, registry)
            }
        }

        IrType::TypeParam(name) => {
            panic!("unsubstituted TypeParam '{name}' — add it to MonoEnv")
        }

        IrType::Tuple(elems) => {
            match elems.len() {
                0 => LirType::Bool,
                1 => ir_type_to_lir_inner(&elems[0], registry),
                _ => {
                    // Look up previously registered synthetic tuple struct.
                    let lir_elems: Vec<LirType> = elems.iter()
                        .map(|e| ir_type_to_lir_inner(e, registry))
                        .collect();
                    let name = tuple_struct_name(&lir_elems);
                    match registry.id_for_name(&name) {
                        Some(id) => LirType::Struct(id),
                        None if registry.lenient => LirType::U64,
                        None => panic!(
                            "tuple type {:?} not registered — call register_tuples_in_type first",
                            elems,
                        ),
                    }
                }
            }
        }

        other => {
            unimplemented!("ir_type_to_lir: unsupported type {:?}", other)
        }
    }
}

pub fn primitive_to_lir(p: PrimitiveType) -> LirType {
    match p {
        PrimitiveType::Bool | PrimitiveType::Bit => LirType::Bool,
        PrimitiveType::U8 | PrimitiveType::BitsInBytes => LirType::U8,
        // Galois maps to Native(AES8) so the C backend can emit GF(2^8) arithmetic
        // (XOR for add/sub, carry-less multiply for mul) rather than wrapping integers.
        PrimitiveType::Galois => LirType::Native(NativeType::AES8),
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
        // Ptr is a single scalar (one machine-word address).
        LirType::Ptr(_) => 1,
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
        // Ptr is a single scalar (one machine-word address).
        LirType::Ptr(_) => vec![ty.clone()],
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

// ============================================================================
// EnumRegistry
// ============================================================================

/// Flat-scalar representation of one enum variant's payload.
#[derive(Clone, Debug)]
pub struct VariantEntry {
    /// Variant name (e.g. "Some", "Ok", "None").
    pub name: String,
    /// Discriminant tag value emitted as a U8 scalar.
    pub discriminant: u64,
    /// Flat LirTypes of this variant's payload scalars (after mono + flatten).
    pub payload_lir_tys: Vec<LirType>,
    /// Field names for struct variants; `None` for tuple/unit variants.
    pub field_names: Option<Vec<String>>,
}

pub struct EnumEntry {
    pub variants: Vec<VariantEntry>,
    /// `max over variants of payload_lir_tys.len()`
    pub max_payload_width: usize,
    /// `StructId` of the synthetic `__Enum_<kind>` struct registered for type lookup.
    pub synthetic_id: StructId,
}

/// Maps enum kind names to their flat-scalar layout info.
pub struct EnumRegistry {
    enums: BTreeMap<String, EnumEntry>,
}

impl EnumRegistry {
    fn new() -> Self {
        EnumRegistry { enums: BTreeMap::new() }
    }

    /// An empty registry (no enums registered).
    pub fn empty() -> Self {
        Self::new()
    }

    /// Return `true` if `kind` is a known enum.
    pub fn is_enum(&self, kind: &StructKind) -> bool {
        self.enums.contains_key(&kind_name(kind))
    }

    /// Return the `StructId` of the synthetic flat struct for this enum, if registered.
    pub fn synthetic_id_for(&self, kind: &StructKind) -> Option<StructId> {
        self.enums.get(&kind_name(kind)).map(|e| e.synthetic_id)
    }

    /// Total flat width (tag + max payload) of this enum kind.
    pub fn flat_width_for(&self, kind: &StructKind) -> Option<usize> {
        self.enums.get(&kind_name(kind)).map(|e| 1 + e.max_payload_width)
    }

    /// Look up a variant by `"EnumName::VariantName"` or `"VariantName"` (searches all enums).
    pub fn find_variant(&self, name: &str) -> Option<(&EnumEntry, &VariantEntry)> {
        // Try "EnumKind::VariantName" form.
        if let Some((enum_name, variant_name)) = name.split_once("::") {
            if let Some(entry) = self.enums.get(enum_name) {
                if let Some(v) = entry.variants.iter().find(|v| v.name == variant_name) {
                    return Some((entry, v));
                }
            }
        }
        // Try matching variant name across all enums (for unqualified names).
        for entry in self.enums.values() {
            if let Some(v) = entry.variants.iter().find(|v| v.name == name) {
                return Some((entry, v));
            }
        }
        None
    }

    /// Variant payload width (scalars only, not including the tag).
    pub fn variant_payload_width(&self, kind: &StructKind, variant_name: &str) -> usize {
        self.enums.get(&kind_name(kind))
            .and_then(|e| e.variants.iter().find(|v| v.name == variant_name))
            .map(|v| v.payload_lir_tys.len())
            .unwrap_or(0)
    }

    /// All variants for a given enum kind.
    pub fn variants_for(&self, kind: &StructKind) -> Option<&[VariantEntry]> {
        self.enums.get(&kind_name(kind)).map(|e| e.variants.as_slice())
    }

    /// Maximum payload width (excluding tag) for a given enum kind.
    pub fn max_payload_width(&self, kind: &StructKind) -> usize {
        self.enums.get(&kind_name(kind)).map(|e| e.max_payload_width).unwrap_or(0)
    }
}

/// Build an `EnumRegistry` from an `IrModule`, applying `env` substitutions.
///
/// For each enum in `module.enums`:
/// 1. Maps variant field types to flat `LirType` lists (with mono via `env`).
/// 2. Computes `max_payload_width = max over all variants of their flat scalar counts`.
/// 3. Registers a synthetic struct `__Enum_{kind}` in `struct_registry` so that
///    `ir_type_to_lir` can return a `LirType::Struct(id)` for enum-typed values.
pub fn build_enum_registry<T: LirTarget>(
    enums: &[IrEnum],
    struct_registry: &mut StructRegistry,
    target: &mut T,
    env: &MonoEnv,
) -> EnumRegistry {
    let mut registry = EnumRegistry::new();

    for ir_enum in enums {
        let name = kind_name(&ir_enum.kind);

        let mut variant_entries: Vec<VariantEntry> = Vec::new();
        for (disc, variant) in ir_enum.variants.iter().enumerate() {
            let (payload_lir_tys, field_names) = match &variant.fields {
                IrEnumVariantData::Unit => (vec![], None),
                IrEnumVariantData::Tuple(tys) => {
                    let lir_tys: Vec<LirType> = tys.iter()
                        .flat_map(|t| {
                            let lir = ir_type_to_lir_inner(&mono_type(t, env), struct_registry);
                            flatten_scalar_types(&lir, struct_registry)
                        })
                        .collect();
                    (lir_tys, None)
                }
                IrEnumVariantData::Struct(fields) => {
                    let mut all_tys = vec![];
                    let names: Vec<String> = fields.iter().map(|f| f.name.clone()).collect();
                    for f in fields {
                        let lir = ir_type_to_lir_inner(&mono_type(&f.ty, env), struct_registry);
                        all_tys.extend(flatten_scalar_types(&lir, struct_registry));
                    }
                    (all_tys, Some(names))
                }
            };
            variant_entries.push(VariantEntry {
                name: variant.name.clone(),
                discriminant: disc as u64,
                payload_lir_tys,
                field_names,
            });
        }

        let max_payload_width = variant_entries.iter()
            .map(|v| v.payload_lir_tys.len())
            .max()
            .unwrap_or(0);

        // Register a synthetic struct: {_tag: U8, _p0: U8, ...} with 1 + max_payload_width fields.
        // We use U8 for all payload slots as a uniform scalar placeholder — actual types
        // are tracked in VariantEntry.payload_lir_tys for lowering.
        let synthetic_name = format!("__Enum_{name}");
        let mut fields = vec![FieldDef { name: "_tag".into(), ty: LirType::U8 }];
        for i in 0..max_payload_width {
            fields.push(FieldDef { name: format!("_p{i}"), ty: LirType::U8 });
        }
        let synthetic_id = target.define_struct(StructDef {
            name: synthetic_name.clone(),
            fields: fields.clone(),
        });
        let field_names = fields.iter().map(|f| f.name.clone()).collect();
        let field_types = fields.into_iter().map(|f| f.ty).collect();
        struct_registry.register_synthetic(synthetic_name.clone(), synthetic_id, field_names, field_types);

        // Also register the enum kind name → synthetic_id so ir_type_to_lir can find it.
        struct_registry.register_synthetic(
            name.clone(),
            synthetic_id,
            // Field names/types not needed here since enum lookup uses EnumRegistry.
            vec!["_tag".into()],
            vec![LirType::U8],
        );

        registry.enums.insert(name, EnumEntry {
            variants: variant_entries,
            max_payload_width,
            synthetic_id,
        });
    }

    registry
}
