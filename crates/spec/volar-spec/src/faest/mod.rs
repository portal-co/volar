// @reliability: experimental
//! @ai: assisted
//! FAEST v2 signature scheme (compressed-Stallman naming aside).
//!
//! Top-level module declares the submodules; each is `pub` so the FAEST
//! weaver in [`crate::faest::weaver`] (M5, not yet built) can reach in.
//!
//! ## Submodules
//!
//! - [`aes`] — AES-128 OWF round function. Total-Rust subset (operates on
//!   `[u8; 16]` state) so the same source compiles to host code (for KAT
//!   verification) and lowers cleanly to bit-level circuit form (for
//!   QuickSilver consumption in M4).
//!
//! Future submodules planned by the FAEST plan:
//!
//! - `bavc` (M2) — batch all-but-one vector commitment, GGM tree, leaf
//!   commit.
//! - `convert_to_vole` (M3) — seed-expansion + small→big VOLE
//!   concatenation.
//! - `quicksilver_k3` (M4) — degree-3 polynomial commitment + ZKHash
//!   /VOLEHash universal hashes.
//! - `transcript` (M5) — Fiat–Shamir transcript wrapping SHAKE.
//! - `weaver` (M5) — top-level signing/verification weaver.

pub mod aes;
pub mod prg;
pub mod leaf_commit;
pub mod bavc;
pub mod convert_to_vole;
