#![no_std]

use cipher::{ArrayLength, BlockCipher, generic_array::GenericArray};
use digest::Digest;

use cipher::generic_array::{functional::FunctionalSequence, sequence::GenericSequence};

pub mod hash_commitment;