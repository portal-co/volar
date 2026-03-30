#![no_std]
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
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