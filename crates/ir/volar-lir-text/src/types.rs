// @reliability: experimental
// @ai: assisted
//! [`WriteText`] implementations for `LirType`, `StructDef`, `FieldDef`,
//! and `IcmpPred`.

use crate::WriteText;
use core::fmt;
use volar_lir::{FieldDef, IcmpPred, LirType, StructDef};
use volar_ir_common::Type as NativeType;

// ============================================================================
// LirType
// ============================================================================

impl WriteText for LirType {
    fn write_text(&self, w: &mut dyn fmt::Write) -> fmt::Result {
        match self {
            LirType::Bool    => w.write_str("bool"),
            LirType::I8      => w.write_str("i8"),
            LirType::U8      => w.write_str("u8"),
            LirType::I16     => w.write_str("i16"),
            LirType::U16     => w.write_str("u16"),
            LirType::I32     => w.write_str("i32"),
            LirType::U32     => w.write_str("u32"),
            LirType::I64     => w.write_str("i64"),
            LirType::U64     => w.write_str("u64"),
            LirType::Arr(elem, len) => {
                w.write_str("arr[")?;
                elem.write_text(w)?;
                write!(w, ", {}]", len)
            }
            LirType::Struct(id) => write!(w, "struct:{}", id),
            LirType::Native(nt) => {
                w.write_str("native:")?;
                write_native_type(nt, w)
            }
            LirType::Ptr(inner) => {
                w.write_str("ptr[")?;
                inner.write_text(w)?;
                w.write_char(']')
            }
        }
    }
}

fn write_native_type(nt: &NativeType, w: &mut dyn fmt::Write) -> fmt::Result {
    match nt {
        NativeType::Bit      => w.write_str("bit"),
        NativeType::_8       => w.write_str("u8"),
        NativeType::_16      => w.write_str("u16"),
        NativeType::_32      => w.write_str("u32"),
        NativeType::_64      => w.write_str("u64"),
        NativeType::_128     => w.write_str("u128"),
        NativeType::_256     => w.write_str("u256"),
        NativeType::AES8     => w.write_str("aes8"),
        NativeType::Galois64 => w.write_str("galois64"),
        // Catch-all for any future variants added under #[non_exhaustive]
        _ => w.write_str("native:unknown"),
    }
}

// ============================================================================
// FieldDef
// ============================================================================

impl WriteText for FieldDef {
    fn write_text(&self, w: &mut dyn fmt::Write) -> fmt::Result {
        w.write_char('(')?;
        self.ty.write_text(w)?;
        write!(w, ", ")?;
        write_quoted_str(&self.name, w)?;
        w.write_char(')')
    }
}

// ============================================================================
// StructDef
// ============================================================================

impl WriteText for StructDef {
    /// Writes: `name=<str> fields=[<field>, ...]`
    /// (the `define_struct id=N` prefix is written by `LirCall::WriteText`)
    fn write_text(&self, w: &mut dyn fmt::Write) -> fmt::Result {
        w.write_str("name=")?;
        write_quoted_str(&self.name, w)?;
        w.write_str(" fields=[")?;
        for (i, f) in self.fields.iter().enumerate() {
            if i > 0 {
                w.write_str(", ")?;
            }
            f.write_text(w)?;
        }
        w.write_char(']')
    }
}

// ============================================================================
// IcmpPred
// ============================================================================

impl WriteText for IcmpPred {
    fn write_text(&self, w: &mut dyn fmt::Write) -> fmt::Result {
        match self {
            IcmpPred::Eq  => w.write_str("eq"),
            IcmpPred::Ne  => w.write_str("ne"),
            IcmpPred::Ult => w.write_str("ult"),
            IcmpPred::Ule => w.write_str("ule"),
            IcmpPred::Ugt => w.write_str("ugt"),
            IcmpPred::Uge => w.write_str("uge"),
            IcmpPred::Slt => w.write_str("slt"),
            IcmpPred::Sle => w.write_str("sle"),
            IcmpPred::Sgt => w.write_str("sgt"),
            IcmpPred::Sge => w.write_str("sge"),
        }
    }
}

// ============================================================================
// Helper: write an escaped quoted string
// ============================================================================

pub(crate) fn write_quoted_str(s: &str, w: &mut dyn fmt::Write) -> fmt::Result {
    w.write_char('"')?;
    for ch in s.chars() {
        match ch {
            '\\' => w.write_str("\\\\")?,
            '"'  => w.write_str("\\\"")?,
            '\n' => w.write_str("\\n")?,
            '\t' => w.write_str("\\t")?,
            '\r' => w.write_str("\\r")?,
            c    => w.write_char(c)?,
        }
    }
    w.write_char('"')
}

/// Write an optional `LirType`, using the literal `none` for `None`.
pub(crate) fn write_opt_lir_type(opt: &Option<LirType>, w: &mut dyn fmt::Write) -> fmt::Result {
    match opt {
        None    => w.write_str("none"),
        Some(t) => t.write_text(w),
    }
}

/// Write a `u32` list as `[a, b, c]`.
pub(crate) fn write_u32_list(list: &[u32], w: &mut dyn fmt::Write) -> fmt::Result {
    w.write_char('[')?;
    for (i, v) in list.iter().enumerate() {
        if i > 0 { w.write_str(", ")?; }
        write!(w, "{}", v)?;
    }
    w.write_char(']')
}

/// Write a `Vec<Vec<u32>>` list as `[[a, b], [c], ...]`.
pub(crate) fn write_u32_list_list(ll: &[alloc::vec::Vec<u32>], w: &mut dyn fmt::Write) -> fmt::Result {
    w.write_char('[')?;
    for (i, inner) in ll.iter().enumerate() {
        if i > 0 { w.write_str(", ")?; }
        write_u32_list(inner, w)?;
    }
    w.write_char(']')
}

/// Write a list of `LirType`s as `[t1, t2, ...]`.
pub(crate) fn write_lir_type_list(tys: &[LirType], w: &mut dyn fmt::Write) -> fmt::Result {
    w.write_char('[')?;
    for (i, t) in tys.iter().enumerate() {
        if i > 0 { w.write_str(", ")?; }
        t.write_text(w)?;
    }
    w.write_char(']')
}
