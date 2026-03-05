#![no_std]

use cipher::{ArrayLength, generic_array::GenericArray};
use digest::Digest;

use cipher::generic_array::functional::FunctionalSequence;

pub mod hash_commitment;
pub mod length_doubling;
