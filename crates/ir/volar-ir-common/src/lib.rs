#![no_std]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord,Hash, Debug)]
#[non_exhaustive]
pub enum Type {
    _32,
    _64,
    _16,
    _8,
    AES8,
    _128,
    _256,
}
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord,Hash, Debug)]
pub struct Constant{
    pub hi: u128,
    pub lo: u128,
}