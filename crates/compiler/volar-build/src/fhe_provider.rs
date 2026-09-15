// @pinnedness: unpinned
// @stability: very-unstable
//! Deterministic LLVM artifact plumbing for future deferred-compute providers.
//!
//! This module intentionally contains no FHE scheme, ciphertext/key type,
//! encryption, evaluation, decryption, or `FheScheme` implementation. It
//! builds an explicit no-std Rust command that emits LLVM bitcode for the
//! stable baseline target `wasm32v1-none`; callers then use the existing
//! `Pipeline::<VaffleStage>::from_command` / `from_command_inlined` path to
//! import the artifact structurally.
//!
//! A future reviewed FHE provider and a future practical heavy-garbling
//! implementation share this toolchain seam. They differ only in the artifact
//! source and public entry ABI, not in how LLVM enters the protocol compiler.
//!
//! # Ledger
//!
//! See `docs/fhe/future-provider-integration-ledger.md` before adding a test,
//! check, or adapter. Every intentionally deferred check has a nearby
//! `TODO(provider-ledger: ...)` marker.

use std::ffi::OsString;
use std::path::{Path, PathBuf};

use volar_ir_build::CommandBuild;

/// Fixed baseline target for provider artifacts.
///
/// `wasm32v1-none` has no host OS, libc, or host CPU selection and therefore
/// gives the provider compiler a deterministic baseline ABI. It is an LLVM
/// code-generation target only; an imported artifact is not executed as WASM
/// by this module.
pub const PROVIDER_LLVM_TARGET: &str = "wasm32v1-none";

/// A source artifact that can be structurally imported as a deferred-compute
/// provider after it compiles to LLVM bitcode. The role is intentionally broad:
/// a reviewed FHE implementation and a practical heavy-garbling implementation
/// can both occupy it.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum DeferredComputeKind {
    /// A future homomorphic-evaluation provider.
    Fhe,
    /// A future high-cost garbling implementation imported through the same
    /// deterministic LLVM toolchain rather than a bespoke compiler path.
    HeavyGarbling,
}

/// Public description of one provider artifact build.
///
/// `entry_points` are public ABI names only. This struct deliberately contains
/// no scheme parameters, FHE key material, plaintext/ciphertext representation,
/// or trust claim. The later protocol adapter validates those separately.
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ProviderArtifactSpec {
    pub kind: DeferredComputeKind,
    pub source: PathBuf,
    pub output_bitcode: PathBuf,
    pub entry_points: Vec<String>,
    /// Extra explicit rustc flags. They may not override the deterministic
    /// target, emit mode, or safety flags chosen by this module.
    pub extra_rustc_args: Vec<OsString>,
}

/// Configuration errors detected before spawning an external compiler.
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ProviderArtifactError {
    EmptyEntryPoints,
    InvalidEntryPoint(String),
    ReservedRustcArgument(OsString),
    SourceEqualsOutput,
}

impl core::fmt::Display for ProviderArtifactError {
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        match self {
            Self::EmptyEntryPoints => {
                f.write_str("provider artifact needs at least one entry point")
            }
            Self::InvalidEntryPoint(entry) => write!(f, "invalid provider entry point `{entry}`"),
            Self::ReservedRustcArgument(arg) => write!(
                f,
                "provider artifact extra rustc argument `{}` overrides a fixed deterministic option",
                arg.to_string_lossy()
            ),
            Self::SourceEqualsOutput => {
                f.write_str("provider source and bitcode output must differ")
            }
        }
    }
}

impl std::error::Error for ProviderArtifactError {}

impl ProviderArtifactSpec {
    /// Make the explicit no-std Rust-to-LLVM-bitcode command consumed by the
    /// existing structural LLVM import pipeline.
    ///
    /// The command does not execute a provider and makes no artifact-validity
    /// assertion. The LTO archive loader accepts raw `.bc`, so no archive or
    /// host linker is needed.
    // TODO(provider-ledger: FHE-PLUMB-TOOLCHAIN-01): compile a minimal
    // no-std fixture after this active rustc has `wasm32v1-none` core support.
    // TODO(provider-ledger: FHE-PLUMB-TOOLCHAIN-02): import the artifact via
    // `Pipeline::from_command` using a reviewed fixture/public ABI.
    pub fn command_build(
        &self,
        rustc: impl AsRef<Path>,
    ) -> Result<CommandBuild, ProviderArtifactError> {
        self.validate()?;
        let mut args = vec![
            OsString::from("--edition=2024"),
            OsString::from("--crate-type=lib"),
            OsString::from("--target"),
            OsString::from(PROVIDER_LLVM_TARGET),
            OsString::from("--emit=llvm-bc"),
            OsString::from("-Copt-level=2"),
            OsString::from("-Ccodegen-units=1"),
            OsString::from("-Cpanic=abort"),
            OsString::from("-Cdebuginfo=0"),
            OsString::from("-Coverflow-checks=on"),
            OsString::from("-o"),
            self.output_bitcode.clone().into_os_string(),
            self.source.clone().into_os_string(),
        ];
        args.extend(self.extra_rustc_args.iter().cloned());
        Ok(CommandBuild {
            program: rustc.as_ref().to_path_buf(),
            args,
            output: self.output_bitcode.clone(),
            cwd: None,
        })
    }

    /// Validate the portion of the artifact contract that is independent of a
    /// concrete provider implementation.
    pub fn validate(&self) -> Result<(), ProviderArtifactError> {
        if self.entry_points.is_empty() {
            return Err(ProviderArtifactError::EmptyEntryPoints);
        }
        for entry in &self.entry_points {
            if !is_llvm_entry_name(entry) {
                return Err(ProviderArtifactError::InvalidEntryPoint(entry.clone()));
            }
        }
        if self.source == self.output_bitcode {
            return Err(ProviderArtifactError::SourceEqualsOutput);
        }
        for arg in &self.extra_rustc_args {
            if is_reserved_rustc_argument(arg) {
                return Err(ProviderArtifactError::ReservedRustcArgument(arg.clone()));
            }
        }
        Ok(())
    }
}

fn is_llvm_entry_name(value: &str) -> bool {
    !value.is_empty()
        && value
            .bytes()
            .all(|byte| byte.is_ascii_alphanumeric() || matches!(byte, b'_' | b'.' | b'$'))
}

fn is_reserved_rustc_argument(value: &OsString) -> bool {
    matches!(
        value.to_string_lossy().as_ref(),
        "--target"
            | "--emit"
            | "--crate-type"
            | "--edition"
            | "-o"
            | "-Copt-level"
            | "-Ccodegen-units"
            | "-Cpanic"
            | "-Cdebuginfo"
            | "-Coverflow-checks"
    )
}
