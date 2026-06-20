#![no_std]

use digest::Digest;
pub use hybrid_array::{Array, ArraySize};

pub mod hash_commitment;
pub mod length_doubling;

pub trait Tracer<Ct, Idx> {
    type Payload;
    fn tick(self, payload: Option<Self::Payload>) -> TracerResult<Self, Ct, Idx>
    where
        Self: Sized;
}
pub enum TracerResult<Tracer, Ct, Idx> {
    ResumeWithCt {
        tracer: Tracer,
        ct: Ct,
    },
    Finished {
        idx: Idx,
        r#continue: Option<Tracer>,
    },
}
