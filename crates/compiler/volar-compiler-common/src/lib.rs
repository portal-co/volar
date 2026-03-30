#![no_std]

use core::error::Error;
extern crate alloc;
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Import {
    Action {
        num_inputs: usize,
        num_outputs: usize,
    },
    Oracle {
        num_inputs: usize,
        num_outputs: usize,
    },
}
pub trait Stitcher<Target, Context> {
    type Output;
    type Error: Error;
    fn stitch(&mut self, target: Target, context: Context) -> Result<Self::Output, Self::Error>;
}
pub trait Transferrable<S, Context> {
    type Output;
    type NewContext;
    type Error: Error;
    fn transfer(
        self,
        stitcher: &mut S,
        context: Context,
    ) -> Result<(Self::Output, Self::NewContext), Self::Error>;
}
impl<S, Context, T: Transferrable<S, Context>, U: Transferrable<S, T::NewContext, Error = T::Error>>
    Transferrable<S, Context> for (T, U)
{
    type Output = (T::Output, U::Output);
    type NewContext = U::NewContext;
    type Error = T::Error;
    fn transfer(
        self,
        stitcher: &mut S,
        context: Context,
    ) -> Result<(Self::Output, Self::NewContext), Self::Error> {
        let (a, b) = self;
        let (a_out, a_ctx) = a.transfer(stitcher, context)?;
        let (b_out, b_ctx) = b.transfer(stitcher, a_ctx)?;
        Ok(((a_out, b_out), b_ctx))
    }
}
