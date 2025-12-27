#![no_std]

use cipher::{ArrayLength, BlockCipher, generic_array::GenericArray};
use digest::Digest;

use cipher::generic_array::{functional::FunctionalSequence, sequence::GenericSequence};

pub mod simple;
pub mod vole;
pub mod byte_gen;
pub use volar_primitives as field;