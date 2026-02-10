#![no_std]

use cipher::{ArrayLength, BlockCipher, generic_array::GenericArray};
use digest::Digest;

use cipher::generic_array::{functional::FunctionalSequence, sequence::GenericSequence};

pub use volar_common::hash_commitment as simple;
pub mod byte_gen;
pub mod vole;
pub mod xsat;
pub use volar_primitives as field;
