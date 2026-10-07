// @pinnedness: unpinned
// @stability: very-unstable
//! @ai: assisted
//!
//! Bridge per-input-bit side metadata to the session layer's input partition.
//!
//! The vc lowering tags each input bit with a [`SideId`] naming which party
//! the value belongs to. This module resolves those sides — from the **local
//! party's** perspective — into [`InputOwner`]s for the MPC session:
//!
//! - **public** → [`InputOwner::Public`] (known to both; no label transfer);
//! - **local / private** → [`InputOwner::Garbler`] (the local party's own
//!   private input; it garbles and sends the selected label);
//! - **remote / blind** → [`InputOwner::Evaluator`] (the other party's input;
//!   delivered via 1-of-2 OT so the garbler never sees it).
//!
//! This is the embedder-side mirror of `volar-vaffle-target`'s
//! `VcProtection { Public, Private, Blind }` vocabulary, restated as
//! [`InputOwner`] so the embedder crate does not depend on the target crate's
//! handler. Roles are named (garbler/evaluator), not machines: which physical
//! party garbles is a session choice — see [`crate::embedder`].

use alloc::vec::Vec;

use volar_mpc::InputOwner;
use volar_side::SideId;

/// Resolve `num_inputs` per-bit sides into a session [`InputOwner`] vector.
///
/// `side_of` maps an input-bit index to its optional [`SideId`]. The three
/// `public`/`local`/`remote` ids name the interned side for each visibility;
/// a bit whose side is `None`, or equal to `public`, is public. Returns the
/// owner vector the session entry points consume.
///
/// This is deliberately a flat per-bit resolver rather than a
/// `SideHandler<Protection = _>`: the embedder already knows its three sides,
/// and the session layer wants an owner per wire, not a protection lattice.
pub fn partition_from_sides(
    num_inputs: usize,
    public: SideId,
    local: SideId,
    remote: SideId,
    side_of: impl Fn(usize) -> Option<SideId>,
) -> Vec<InputOwner> {
    (0..num_inputs)
        .map(|i| match side_of(i) {
            Some(s) if s == local => InputOwner::Garbler,
            Some(s) if s == remote => InputOwner::Evaluator,
            // `None` and `public` are both known-to-both.
            _ => {
                let _ = public;
                InputOwner::Public
            }
        })
        .collect()
}
