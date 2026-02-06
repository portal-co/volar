// Cleaned-up expanded source of volar-primitives for the volar compiler.
// Generated from `cargo expand -p volar-primitives` with manual cleanup
// to remove ::core:: paths, #[automatically_derived] attrs, and other
// constructs the volar parser doesn't handle.
//
// This captures the essential public API: struct definitions, trait
// definitions, and trait implementations.

pub trait Invert {
    fn invert(&self) -> Self;
}

pub struct Bit(pub bool);

impl Add<Bit> for Bit {
    type Output = Bit;
    fn add(self, rhs: Bit) -> Self::Output {
        todo!()
    }
}

impl Sub<Bit> for Bit {
    type Output = Bit;
    fn sub(self, rhs: Bit) -> Self::Output {
        todo!()
    }
}

impl Mul<Bit> for Bit {
    type Output = Bit;
    fn mul(self, rhs: Bit) -> Self::Output {
        todo!()
    }
}

impl BitXor<u8> for Bit {
    type Output = Bit;
    fn bitxor(self, rhs: u8) -> Self::Output {
        todo!()
    }
}

pub struct Galois(pub u8);

impl Add<Galois> for Galois {
    type Output = Galois;
    fn add(self, rhs: Galois) -> Self::Output {
        todo!()
    }
}

impl Mul<Galois> for Galois {
    type Output = Galois;
    fn mul(self, rhs: Galois) -> Self::Output {
        todo!()
    }
}

impl Sub<Galois> for Galois {
    type Output = Galois;
    fn sub(self, rhs: Galois) -> Self::Output {
        todo!()
    }
}

impl BitXor<u8> for Galois {
    type Output = Galois;
    fn bitxor(self, rhs: u8) -> Self::Output {
        todo!()
    }
}

impl Invert for Galois {
    fn invert(&self) -> Self {
        todo!()
    }
}

pub struct BitsInBytes(pub u8);

impl Add<BitsInBytes> for BitsInBytes {
    type Output = BitsInBytes;
    fn add(self, rhs: BitsInBytes) -> Self::Output {
        todo!()
    }
}

impl Mul<BitsInBytes> for BitsInBytes {
    type Output = BitsInBytes;
    fn mul(self, rhs: BitsInBytes) -> Self::Output {
        todo!()
    }
}

impl Sub<BitsInBytes> for BitsInBytes {
    type Output = BitsInBytes;
    fn sub(self, rhs: BitsInBytes) -> Self::Output {
        todo!()
    }
}

impl BitXor<u8> for BitsInBytes {
    type Output = BitsInBytes;
    fn bitxor(self, rhs: u8) -> Self::Output {
        todo!()
    }
}

pub struct Galois64(pub u64);

impl Add<Galois64> for Galois64 {
    type Output = Galois64;
    fn add(self, rhs: Galois64) -> Self::Output {
        todo!()
    }
}

impl Mul<Galois64> for Galois64 {
    type Output = Galois64;
    fn mul(self, rhs: Galois64) -> Self::Output {
        todo!()
    }
}

impl Sub<Galois64> for Galois64 {
    type Output = Galois64;
    fn sub(self, rhs: Galois64) -> Self::Output {
        todo!()
    }
}

impl BitXor<u8> for Galois64 {
    type Output = Galois64;
    fn bitxor(self, rhs: u8) -> Self::Output {
        todo!()
    }
}

pub struct BitsInBytes64(pub u64);

impl Add<BitsInBytes64> for BitsInBytes64 {
    type Output = BitsInBytes64;
    fn add(self, rhs: BitsInBytes64) -> Self::Output {
        todo!()
    }
}

impl Mul<BitsInBytes64> for BitsInBytes64 {
    type Output = BitsInBytes64;
    fn mul(self, rhs: BitsInBytes64) -> Self::Output {
        todo!()
    }
}

impl Sub<BitsInBytes64> for BitsInBytes64 {
    type Output = BitsInBytes64;
    fn sub(self, rhs: BitsInBytes64) -> Self::Output {
        todo!()
    }
}

impl BitXor<u8> for BitsInBytes64 {
    type Output = BitsInBytes64;
    fn bitxor(self, rhs: u8) -> Self::Output {
        todo!()
    }
}

pub struct Tropical<T>(pub T);

impl<T: Ord> Add<Tropical<T>> for Tropical<T> {
    type Output = Tropical<T>;
    fn add(self, rhs: Tropical<T>) -> Self::Output {
        todo!()
    }
}

impl<T: Add<U>, U> Mul<Tropical<U>> for Tropical<T> {
    type Output = Tropical<<T as Add<U>>::Output>;
    fn mul(self, rhs: Tropical<U>) -> Self::Output {
        todo!()
    }
}
