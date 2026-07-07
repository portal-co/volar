// @reliability: normal
// @ai: assisted
//! Reentry complexity hints — well-founded descent metadata on branch targets.

extern crate alloc;

use alloc::{boxed::Box, string::String, vec, vec::Vec};

/// Attached to a branch target: measures that MUST decrease on re-entry via this edge.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[cfg_attr(feature = "rkyv", rkyv(serialize_bounds(
    __S: rkyv::ser::Writer + rkyv::ser::Allocator,
    __S::Error: rkyv::rancor::Source,
)))]
#[cfg_attr(feature = "rkyv", rkyv(bytecheck(bounds(
    __C: rkyv::validation::ArchiveContext,
    __C::Error: rkyv::rancor::Source,
))))]
#[cfg_attr(feature = "rkyv", rkyv(deserialize_bounds(__D::Error: rkyv::rancor::Source)))]
pub struct ReentryHint {
    #[cfg_attr(feature = "rkyv", rkyv(omit_bounds))]
    pub measures: Vec<MeasureSpec>,
}

/// Portable reference to a compiler struct (module path + name).
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
pub struct StructRef {
    pub module_path: Vec<String>,
    pub name: String,
}

/// Recursive well-founded measure over target-block params.
#[derive(Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
#[cfg_attr(feature = "rkyv", derive(rkyv::Archive, rkyv::Serialize, rkyv::Deserialize))]
#[non_exhaustive]
#[cfg_attr(feature = "rkyv", rkyv(serialize_bounds(
    __S: rkyv::ser::Writer + rkyv::ser::Allocator,
    __S::Error: rkyv::rancor::Source,
)))]
#[cfg_attr(feature = "rkyv", rkyv(bytecheck(bounds(
    __C: rkyv::validation::ArchiveContext,
    __C::Error: rkyv::rancor::Source,
))))]
#[cfg_attr(feature = "rkyv", rkyv(deserialize_bounds(__D::Error: rkyv::rancor::Source)))]
pub enum MeasureSpec {
    /// Scalar param strictly decreases each re-entry.
    Strict { param: usize, signed: bool },

    /// Bit param: only valid re-entry transition is 1 → 0.
    BitToZero { param: usize },

    /// `minuend - subtrahend` strictly decreases.
    Diff {
        #[cfg_attr(feature = "rkyv", rkyv(omit_bounds))]
        minuend: Box<MeasureSpec>,
        #[cfg_attr(feature = "rkyv", rkyv(omit_bounds))]
        subtrahend: Box<MeasureSpec>,
        signed: bool,
    },

    /// Lexicographic product over one or more params.
    Digits {
        params: Vec<usize>,
        #[cfg_attr(feature = "rkyv", rkyv(omit_bounds))]
        elements: Vec<MeasureSpec>,
    },

    /// Compiler struct-typed param: at least one listed field strictly decreases;
    /// all other fields must be equal on re-entry.
    Structural {
        param: usize,
        struct_ref: StructRef,
        #[cfg_attr(feature = "rkyv", rkyv(omit_bounds))]
        fields: Vec<(usize, MeasureSpec)>,
    },
}

impl ReentryHint {
    /// Canonical ascending bounded-loop back-edge (limit - counter).
    pub fn bounded_loop_ascending() -> Self {
        ReentryHint {
            measures: vec![MeasureSpec::Diff {
                minuend: Box::new(MeasureSpec::Strict { param: 1, signed: false }),
                subtrahend: Box::new(MeasureSpec::Strict { param: 0, signed: false }),
                signed: false,
            }],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use alloc::vec;

    #[test]
    fn bounded_loop_hint_shape() {
        let h = ReentryHint::bounded_loop_ascending();
        assert_eq!(h.measures.len(), 1);
        match &h.measures[0] {
            MeasureSpec::Diff { minuend, subtrahend, signed: false } => {
                assert!(matches!(**minuend, MeasureSpec::Strict { param: 1, signed: false }));
                assert!(matches!(**subtrahend, MeasureSpec::Strict { param: 0, signed: false }));
            }
            _ => panic!("expected Diff"),
        }
    }

    #[test]
    fn digits_lex_nested() {
        let spec = MeasureSpec::Digits {
            params: vec![0, 1],
            elements: vec![
                MeasureSpec::Strict { param: 0, signed: false },
                MeasureSpec::BitToZero { param: 1 },
            ],
        };
        match spec {
            MeasureSpec::Digits { params, elements } => {
                assert_eq!(params, vec![0, 1]);
                assert_eq!(elements.len(), 2);
            }
            _ => panic!("expected Digits"),
        }
    }
}
