#![no_std]

use cipher::{ArrayLength, BlockCipher, generic_array::GenericArray};
use digest::Digest;

use cipher::generic_array::{functional::FunctionalSequence, sequence::GenericSequence};
pub trait Commitment {
    type Opening;
    fn validate(&self, o: &Self::Opening) -> bool;
}
pub mod simple;
pub mod vole;
pub mod byte_gen;
pub mod field;