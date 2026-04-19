// @reliability: experimental
// @ai: assisted
//! Shared helpers for constant-folding all three IR layers.

use alloc::{collections::BTreeMap, vec::Vec};
use volar_ir_common::{Constant, IrType, Stmt, Type, TypeId, TypeTable};

// ============================================================================
// Alias canonicalization
// ============================================================================

/// Follow the alias chain for `v` through `alias_map`, up to 64 hops.
pub fn canon_alias<V: Copy + Ord>(alias_map: &BTreeMap<V, V>, mut v: V) -> V {
    for _ in 0..64 {
        match alias_map.get(&v) {
            Some(&w) if w != v => v = w,
            _ => break,
        }
    }
    v
}

// ============================================================================
// 256-bit constant arithmetic
// ============================================================================

pub fn constant_is_zero(c: Constant) -> bool {
    c.hi == 0 && c.lo == 0
}

/// True if the lowest `width` bits of `c` are all 1 (and width ≤ 256).
pub fn constant_is_all_ones(c: Constant, width: usize) -> bool {
    let m = mask_constant(Constant { hi: u128::MAX, lo: u128::MAX }, width);
    c == m
}

/// Zero out bits above position `width-1`.
pub fn mask_constant(c: Constant, width: usize) -> Constant {
    if width == 0 {
        return Constant { hi: 0, lo: 0 };
    }
    if width >= 256 {
        return c;
    }
    if width >= 128 {
        let hi_bits = width - 128;
        let hi_mask = if hi_bits >= 128 { u128::MAX } else { (1u128 << hi_bits) - 1 };
        Constant { lo: c.lo, hi: c.hi & hi_mask }
    } else {
        let lo_mask = (1u128 << width) - 1;
        Constant { lo: c.lo & lo_mask, hi: 0 }
    }
}

pub fn constant_and(a: Constant, b: Constant) -> Constant {
    Constant { hi: a.hi & b.hi, lo: a.lo & b.lo }
}

pub fn constant_xor(a: Constant, b: Constant) -> Constant {
    Constant { hi: a.hi ^ b.hi, lo: a.lo ^ b.lo }
}

pub fn constant_or(a: Constant, b: Constant) -> Constant {
    Constant { hi: a.hi | b.hi, lo: a.lo | b.lo }
}

/// Shift `c` left by `n` bit positions (into a 256-bit field).
pub fn constant_shl(c: Constant, n: usize) -> Constant {
    if n == 0 {
        return c;
    }
    if n >= 256 {
        return Constant { hi: 0, lo: 0 };
    }
    if n >= 128 {
        Constant { hi: c.lo << (n - 128), lo: 0 }
    } else {
        Constant {
            hi: (c.hi << n) | (c.lo >> (128 - n)),
            lo: c.lo << n,
        }
    }
}

/// Logical shift `c` right by `n` bit positions.
pub fn constant_shr(c: Constant, n: usize) -> Constant {
    if n == 0 {
        return c;
    }
    if n >= 256 {
        return Constant { hi: 0, lo: 0 };
    }
    if n >= 128 {
        Constant { hi: 0, lo: c.hi >> (n - 128) }
    } else {
        Constant {
            hi: c.hi >> n,
            lo: (c.lo >> n) | (c.hi << (128 - n)),
        }
    }
}

/// Rotate the lowest `width` bits of `c` left by `n`.
pub fn constant_rol(c: Constant, width: usize, n: usize) -> Constant {
    if width == 0 || n == 0 {
        return c;
    }
    let n = n % width;
    if n == 0 {
        return c;
    }
    let c = mask_constant(c, width);
    let l = constant_shl(c, n);
    let r = constant_shr(c, width - n);
    mask_constant(constant_or(l, r), width)
}

/// Rotate the lowest `width` bits of `c` right by `n`.
pub fn constant_ror(c: Constant, width: usize, n: usize) -> Constant {
    if width == 0 || n == 0 {
        return c;
    }
    let n = n % width;
    constant_rol(c, width, width - n)
}

// ============================================================================
// Type utilities
// ============================================================================

/// Bit-width of a `Type` (always well-defined for non-field types).
pub fn primitive_type_width(t: Type) -> usize {
    match t {
        Type::Bit => 1,
        Type::_8 | Type::AES8 => 8,
        Type::_16 => 16,
        Type::_32 => 32,
        Type::_64 | Type::Galois64 => 64,
        Type::_128 => 128,
        Type::_256 => 256,
        _ => panic!("primitive_type_width: unknown Type variant"),
    }
}

/// Recursively compute the bit-width of `ty_id`.
/// Returns `None` for `AES8`, `Galois64`, `Block`, and `Func` types.
pub fn type_bit_width(ty_id: TypeId, types: &TypeTable) -> Option<usize> {
    match types.0.get(ty_id.0 as usize)? {
        IrType::Primitive(Type::AES8) | IrType::Primitive(Type::Galois64) => None,
        IrType::Primitive(t) => Some(primitive_type_width(*t)),
        IrType::Vec(n, elem_ty) => Some(n * type_bit_width(*elem_ty, types)?),
        IrType::Tuple(elems) => {
            let mut sum = 0usize;
            for &e in elems {
                sum += type_bit_width(e, types)?;
            }
            Some(sum)
        }
        IrType::Block { .. } | IrType::Func { .. } => None,
    }
}

/// Returns `true` if `ty_id` is `AES8` or `Galois64`.
pub fn is_field_type(ty_id: TypeId, types: &TypeTable) -> bool {
    matches!(
        types.0.get(ty_id.0 as usize),
        Some(IrType::Primitive(Type::AES8)) | Some(IrType::Primitive(Type::Galois64))
    )
}

/// Extract the output `TypeId` from a `Stmt`, if it produces one.
pub fn stmt_output_type<V, A>(stmt: &Stmt<V, A>) -> Option<TypeId> {
    match stmt {
        Stmt::Const(_, ty) => Some(*ty),
        Stmt::Transmute { dst_ty, .. } => Some(*dst_ty),
        Stmt::Poly { ty, .. } => Some(*ty),
        Stmt::Rol { ty, .. } => Some(*ty),
        Stmt::Ror { ty, .. } => Some(*ty),
        Stmt::Merge { ty, .. } => Some(*ty),
        Stmt::Splat { ty, .. } => Some(*ty),
        Stmt::Shuffle { ty, .. } => Some(*ty),
        Stmt::OracleCall { result_ty, .. } => Some(*result_ty),
        Stmt::OracleOutput { ty, .. } => Some(*ty),
        Stmt::ActionCall { result_ty, .. } => Some(*result_ty),
        Stmt::ActionOutput { ty, .. } => Some(*ty),
        Stmt::Rng { ty, .. } => Some(*ty),
        Stmt::StorageRead { ty, .. } => Some(*ty),
        Stmt::StorageWrite { .. } => None,
    }
}

// ============================================================================
// Alias application to Stmt<V, V>
// ============================================================================

/// Rewrite all var references in `stmt` through `alias_map`.
/// For `Poly`, monomial keys are rebuilt (sorted + deduped) after substitution.
/// Returns `true` if any reference was changed.
pub fn apply_aliases_to_stmt<V: Copy + Ord + Clone>(
    stmt: &mut Stmt<V, V>,
    alias_map: &BTreeMap<V, V>,
) -> bool {
    if alias_map.is_empty() {
        return false;
    }
    let mut changed = false;

    match stmt {
        Stmt::StorageRead { addr, .. } => {
            let c = canon_alias(alias_map, *addr);
            if c != *addr { *addr = c; changed = true; }
        }
        Stmt::StorageWrite { src, addr, .. } => {
            let cs = canon_alias(alias_map, *src);
            if cs != *src { *src = cs; changed = true; }
            let ca = canon_alias(alias_map, *addr);
            if ca != *addr { *addr = ca; changed = true; }
        }
        Stmt::Const(_, _) | Stmt::Rng { .. } => {}
        Stmt::Transmute { src, .. } => {
            let c = canon_alias(alias_map, *src);
            if c != *src { *src = c; changed = true; }
        }
        Stmt::Poly { coeffs, .. } => {
            // Rebuild the BTreeMap with aliased, sorted, deduped keys.
            // XOR-accumulate coefficients for colliding keys.
            let old = core::mem::take(coeffs);
            for (key, coeff) in old {
                if coeff & 1 == 0 {
                    changed = true;
                    continue;
                }
                let mut new_key: Vec<V> = key
                    .iter()
                    .map(|&v| {
                        let w = canon_alias(alias_map, v);
                        if w != v {
                            changed = true;
                        }
                        w
                    })
                    .collect();
                new_key.sort();
                let before_len = new_key.len();
                new_key.dedup(); // idempotent AND: v*v = v in GF(2)
                if new_key.len() != before_len {
                    changed = true;
                }
                *coeffs.entry(new_key).or_insert(0) ^= coeff;
            }
            coeffs.retain(|_, c| *c & 1 != 0);
        }
        Stmt::Rol { src, .. } | Stmt::Ror { src, .. } | Stmt::Splat { src, .. } => {
            let c = canon_alias(alias_map, *src);
            if c != *src { *src = c; changed = true; }
        }
        Stmt::Merge { parts, .. } => {
            for p in parts.iter_mut() {
                let c = canon_alias(alias_map, *p);
                if c != *p { *p = c; changed = true; }
            }
        }
        Stmt::Shuffle { result_bits, .. } => {
            for (_, v) in result_bits.iter_mut() {
                let c = canon_alias(alias_map, *v);
                if c != *v { *v = c; changed = true; }
            }
        }
        Stmt::OracleCall { args, .. } => {
            for a in args.iter_mut() {
                let c = canon_alias(alias_map, *a);
                if c != *a { *a = c; changed = true; }
            }
        }
        Stmt::OracleOutput { call, .. } => {
            let c = canon_alias(alias_map, *call);
            if c != *call { *call = c; changed = true; }
        }
        Stmt::ActionCall { guard, args, fallbacks, .. } => {
            let cg = canon_alias(alias_map, *guard);
            if cg != *guard { *guard = cg; changed = true; }
            for a in args.iter_mut() {
                let c = canon_alias(alias_map, *a);
                if c != *a { *a = c; changed = true; }
            }
            for f in fallbacks.iter_mut() {
                let c = canon_alias(alias_map, *f);
                if c != *f { *f = c; changed = true; }
            }
        }
        Stmt::ActionOutput { call, .. } => {
            let c = canon_alias(alias_map, *call);
            if c != *call { *call = c; changed = true; }
        }
    }

    changed
}

// ============================================================================
// GF(2) polynomial folding
// ============================================================================

/// Simplify a `Poly` in-place using known constants and type information.
///
/// Conservative rules applied:
/// - Skip monomials containing `AES8`/`Galois64`-typed vars (leave unchanged).
/// - Drop monomials where any var resolves to the zero constant.
/// - Remove vars that resolve to all-ones from monomial keys (multiplicative identity).
/// - Fold empty-key monomials (after identity removal) into `constant`.
/// - Mask `constant` to the output type width.
///
/// Returns `true` if any change was made.
pub fn fold_poly_in_place<V: Clone + Ord>(
    ty: TypeId,
    coeffs: &mut BTreeMap<Vec<V>, u8>,
    constant: &mut Constant,
    const_map: &BTreeMap<V, Constant>,
    type_map: &BTreeMap<V, TypeId>,
    types: &TypeTable,
) -> bool {
    // Skip field-element output types entirely.
    if is_field_type(ty, types) {
        return false;
    }

    let mut changed = false;
    let old_coeffs = core::mem::take(coeffs);
    // Keep a clone to compare at the end (old_coeffs is consumed by the loop).
    let old_coeffs_for_cmp = old_coeffs.clone();
    let mut new_coeffs: BTreeMap<Vec<V>, u8> = BTreeMap::new();

    for (key, coeff) in old_coeffs {
        if coeff & 1 == 0 {
            changed = true;
            continue;
        }

        // Skip monomials with any field-typed var.
        let has_field = key.iter().any(|v| {
            type_map
                .get(v)
                .map_or(false, |&tid| is_field_type(tid, types))
        });
        if has_field {
            *new_coeffs.entry(key).or_insert(0) ^= coeff;
            continue;
        }

        // Simplify key using const_map.
        let mut monomial_zero = false;
        let mut new_key: Vec<V> = Vec::with_capacity(key.len());

        for v in &key {
            if let Some(&c) = const_map.get(v) {
                let w = type_map
                    .get(v)
                    .and_then(|&tid| type_bit_width(tid, types))
                    .unwrap_or(1);
                let c_masked = mask_constant(c, w);
                if constant_is_zero(c_masked) {
                    // AND with 0 → whole monomial is 0.
                    monomial_zero = true;
                    changed = true;
                    break;
                } else if constant_is_all_ones(c_masked, w) {
                    // AND with all-ones → multiplicative identity, drop var.
                    changed = true;
                } else {
                    // Non-trivial constant — keep conservatively.
                    new_key.push(v.clone());
                }
            } else {
                new_key.push(v.clone());
            }
        }

        if monomial_zero {
            continue;
        }

        new_key.sort();
        let before_len = new_key.len();
        new_key.dedup(); // idempotent AND in GF(2)
        if new_key.len() != before_len || new_key.len() != key.len() {
            changed = true;
        }

        if new_key.is_empty() {
            // Empty monomial (odd coeff) → contribute all-ones to constant.
            let w = type_bit_width(ty, types).unwrap_or(1);
            let all_ones = mask_constant(Constant { hi: u128::MAX, lo: u128::MAX }, w);
            *constant = constant_xor(*constant, all_ones);
            changed = true;
        } else {
            *new_coeffs.entry(new_key).or_insert(0) ^= coeff;
        }
    }

    // Remove zero-coeff entries.
    new_coeffs.retain(|_, c| *c & 1 != 0);

    // Compare against the original (now-moved) coeffs to detect XOR cancellations.
    if new_coeffs != old_coeffs_for_cmp {
        changed = true;
    }
    *coeffs = new_coeffs;

    // Mask constant to output width.
    if let Some(w) = type_bit_width(ty, types) {
        let masked = mask_constant(*constant, w);
        if masked != *constant {
            *constant = masked;
            changed = true;
        }
    }

    changed
}
