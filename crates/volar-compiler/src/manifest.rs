//! Type manifest files for cross-crate compilation.
//!
//! A `.volar.d` manifest captures the *signatures* of a compiled crate
//! (structs, traits, impl blocks, type aliases, function signatures) without
//! function/method bodies.  This is the minimum information needed to
//! type-check and lower code that depends on the crate.
//!
//! ## Format
//!
//! The on-disk representation is **text-based Rust** with `todo!()` bodies,
//! prefixed by a single `0xFF` byte (invalid UTF-8) as a poison pill:
//!
//! - `rustc` rejects it immediately (Rust source must be valid UTF-8)
//! - `syn` rejects it (requires valid UTF-8)
//! - The volar-compiler parser in normal mode rejects it
//! - Only `parse_manifest()` strips the `0xFF` prefix before parsing

use core::fmt::{self, Write};

#[cfg(feature = "std")]
use std::string::{String, ToString};
#[cfg(feature = "std")]
use std::vec::Vec;
#[cfg(feature = "std")]
use std::format;
#[cfg(feature = "std")]
use std::boxed::Box;
#[cfg(feature = "std")]
use std::vec;

#[cfg(not(feature = "std"))]
use alloc::string::{String, ToString};
#[cfg(not(feature = "std"))]
use alloc::vec::Vec;
#[cfg(not(feature = "std"))]
use alloc::format;
#[cfg(not(feature = "std"))]
use alloc::boxed::Box;
#[cfg(not(feature = "std"))]
use alloc::vec;

use crate::ir::*;
use crate::printer::{
    DisplayRust, ExprWriter, FunctionWriter, GenericsWriter, ImplWriter, RustBackend,
    StructWriter, TraitWriter, TypeWriter, WhereClauseWriter,
};

/// First byte of every `.volar.d` file — invalid UTF-8 poison pill.
pub const MANIFEST_MARKER: u8 = 0xFF;

// ============================================================================
// TYPE MANIFEST
// ============================================================================

/// Metadata + IR for a compiled crate's type manifest.
#[derive(Debug, Clone, PartialEq)]
pub struct TypeManifest {
    pub crate_name: String,
    pub version: String,
    pub deps: Vec<String>,
    pub module: IrModule,
}

// ============================================================================
// EMIT
// ============================================================================

/// Strip function/method bodies from an `IrModule`, replacing them with
/// `todo!()`.  Everything else (structs, traits, signatures) is kept.
fn strip_bodies(module: &IrModule) -> IrModule {
    let stub_block = IrBlock {
        stmts: Vec::new(),
        expr: Some(Box::new(IrExpr::Call {
            func: Box::new(IrExpr::Path {
                segments: vec!["todo!".into()],
                type_args: Vec::new(),
            }),
            args: Vec::new(),
        })),
    };

    let strip_function = |f: &IrFunction| -> IrFunction {
        IrFunction {
            name: f.name.clone(),
            generics: f.generics.clone(),
            receiver: f.receiver,
            params: f.params.clone(),
            return_type: f.return_type.clone(),
            where_clause: f.where_clause.clone(),
            body: stub_block.clone(),
        }
    };

    let stripped_impls = module
        .impls
        .iter()
        .map(|imp| IrImpl {
            generics: imp.generics.clone(),
            trait_: imp.trait_.clone(),
            self_ty: imp.self_ty.clone(),
            where_clause: imp.where_clause.clone(),
            items: imp
                .items
                .iter()
                .map(|item| match item {
                    IrImplItem::Method(f) => IrImplItem::Method(strip_function(f)),
                    other => other.clone(),
                })
                .collect(),
        })
        .collect();

    let stripped_fns = module.functions.iter().map(|f| strip_function(f)).collect();

    IrModule {
        name: module.name.clone(),
        structs: module.structs.clone(),
        traits: module.traits.clone(),
        impls: stripped_impls,
        functions: stripped_fns,
        type_aliases: module.type_aliases.clone(),
    }
}

/// Writer for manifest modules — renders signature-only Rust.
struct ManifestModuleWriter<'a> {
    crate_name: &'a str,
    version: &'a str,
    deps: &'a [String],
    module: &'a IrModule,
}

impl<'a> RustBackend for ManifestModuleWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        // Header comments (after 0xFF, which is prepended separately)
        writeln!(f, "//! @volar-manifest {} {}", self.crate_name, self.version)?;
        if !self.deps.is_empty() {
            writeln!(f, "//! @volar-deps [{}]", self.deps.join(", "))?;
        }
        writeln!(f)?;

        for s in &self.module.structs {
            StructWriter { s }.fmt(f)?;
            writeln!(f)?;
        }
        for t in &self.module.traits {
            TraitWriter { t }.fmt(f)?;
            writeln!(f)?;
        }
        for imp in &self.module.impls {
            ImplWriter { i: imp }.fmt(f)?;
            writeln!(f)?;
        }
        for func in &self.module.functions {
            ManifestFunctionWriter { f: func }.fmt(f)?;
            writeln!(f)?;
        }
        for alias in &self.module.type_aliases {
            write!(f, "pub type {}", alias.name)?;
            GenericsWriter {
                generics: &alias.generics,
            }
            .fmt(f)?;
            write!(f, " = ")?;
            TypeWriter { ty: &alias.target }.fmt(f)?;
            writeln!(f, ";")?;
        }

        Ok(())
    }
}

/// Function writer for manifests — renders `todo!()` bodies.
struct ManifestFunctionWriter<'a> {
    f: &'a IrFunction,
}

impl<'a> RustBackend for ManifestFunctionWriter<'a> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "pub fn {}", self.f.name)?;
        GenericsWriter {
            generics: &self.f.generics,
        }
        .fmt(f)?;
        write!(f, "(")?;
        if let Some(r) = self.f.receiver {
            crate::printer::ReceiverWriter { r }.fmt(f)?;
            if !self.f.params.is_empty() {
                write!(f, ", ")?;
            }
        }
        for (i, p) in self.f.params.iter().enumerate() {
            if i > 0 {
                write!(f, ", ")?;
            }
            write!(f, "{}: ", p.name)?;
            TypeWriter { ty: &p.ty }.fmt(f)?;
        }
        write!(f, ")")?;
        if let Some(ret) = &self.f.return_type {
            write!(f, " -> ")?;
            TypeWriter { ty: ret }.fmt(f)?;
        }
        WhereClauseWriter {
            where_clause: &self.f.where_clause,
        }
        .fmt(f)?;
        writeln!(f, " {{ todo!() }}")?;
        Ok(())
    }
}

/// Emit a manifest as raw bytes (with `0xFF` prefix).
///
/// Bodies are stripped; only signatures remain.
pub fn emit_manifest(
    module: &IrModule,
    crate_name: &str,
    version: &str,
    deps: &[String],
) -> Vec<u8> {
    let stripped = strip_bodies(module);

    let writer = ManifestModuleWriter {
        crate_name,
        version,
        deps,
        module: &stripped,
    };

    let text = format!("{}", DisplayRust(writer));
    let mut bytes = Vec::with_capacity(1 + text.len());
    bytes.push(MANIFEST_MARKER);
    bytes.extend_from_slice(text.as_bytes());
    bytes
}

// ============================================================================
// PARSE
// ============================================================================

/// Error type for manifest operations.
#[derive(Debug)]
pub enum ManifestError {
    /// The first byte is not the `0xFF` marker.
    MissingMarker,
    /// The content after the marker is not valid UTF-8.
    InvalidUtf8,
    /// Missing or malformed `//! @volar-manifest` header.
    MissingHeader,
    /// Parse error from the underlying parser.
    ParseError(String),
}

impl fmt::Display for ManifestError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::MissingMarker => write!(f, "not a volar manifest: missing 0xFF marker"),
            Self::InvalidUtf8 => write!(f, "manifest content is not valid UTF-8"),
            Self::MissingHeader => write!(f, "missing //! @volar-manifest header"),
            Self::ParseError(e) => write!(f, "manifest parse error: {}", e),
        }
    }
}

/// Parse a `.volar.d` manifest from raw bytes.
///
/// 1. Verifies `0xFF` marker
/// 2. Strips marker
/// 3. Decodes UTF-8
/// 4. Extracts `//! @volar-manifest` header
/// 5. Parses body with the volar parser
/// 6. Returns `TypeManifest`
#[cfg(feature = "parsing")]
pub fn parse_manifest(raw: &[u8]) -> core::result::Result<TypeManifest, ManifestError> {
    // 1. Check marker
    if raw.first() != Some(&MANIFEST_MARKER) {
        return Err(ManifestError::MissingMarker);
    }

    // 2. Strip marker, decode UTF-8
    let text = core::str::from_utf8(&raw[1..]).map_err(|_| ManifestError::InvalidUtf8)?;

    // 3. Extract header
    let (crate_name, version, deps) = parse_header(text)?;

    // 4. Parse with existing parser
    // The header lines are doc comments that syn will ignore
    let module = crate::parser::parse_source(text, &crate_name)
        .map_err(|e| ManifestError::ParseError(format!("{}", e)))?;

    Ok(TypeManifest {
        crate_name,
        version,
        deps,
        module,
    })
}

/// Extract metadata from `//! @volar-manifest name version` and
/// `//! @volar-deps [dep1, dep2]` header lines.
fn parse_header(text: &str) -> core::result::Result<(String, String, Vec<String>), ManifestError> {
    let mut crate_name = None;
    let mut version = None;
    let mut deps = Vec::new();

    for line in text.lines() {
        let trimmed = line.trim();
        if let Some(rest) = trimmed.strip_prefix("//! @volar-manifest ") {
            let parts: Vec<&str> = rest.split_whitespace().collect();
            if parts.len() >= 2 {
                crate_name = Some(parts[0].to_string());
                version = Some(parts[1].to_string());
            }
        } else if let Some(rest) = trimmed.strip_prefix("//! @volar-deps ") {
            // Parse "[dep1, dep2]"
            let inner = rest.trim().trim_start_matches('[').trim_end_matches(']');
            if !inner.is_empty() {
                deps = inner.split(',').map(|s| s.trim().to_string()).collect();
            }
        } else if !trimmed.starts_with("//!") {
            break; // Stop at first non-doc-comment line
        }
    }

    match (crate_name, version) {
        (Some(name), Some(ver)) => Ok((name, ver, deps)),
        _ => Err(ManifestError::MissingHeader),
    }
}

// ============================================================================
// CONVENIENCE
// ============================================================================

/// Check if raw bytes look like a volar manifest (starts with `0xFF`).
pub fn is_manifest(raw: &[u8]) -> bool {
    raw.first() == Some(&MANIFEST_MARKER)
}
