#![no_std]
extern crate alloc;
#[derive(Clone, Copy,PartialEq, Eq, PartialOrd, Ord,Debug)]
pub enum Import{
    Action{
        num_inputs: usize,
        num_outputs: usize
    },
    Oracle{
        num_inputs: usize,
        num_outputs: usize
    }
}