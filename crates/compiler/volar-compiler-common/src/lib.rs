#![no_std]

use core::error::Error;
extern crate alloc;
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
pub enum Import<Types> {
    Action {
        num_inputs: Types,
        num_outputs: Types,
    },
    Oracle {
        num_inputs: Types,
        num_outputs: Types,
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
    fn reverse(
        this: Self::Output,
        stitcher: &mut S,
        context: Self::NewContext,
    ) -> Result<(Self, Context), Self::Error>
    where
        Self: Sized;
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
    fn reverse(
        this: Self::Output,
        stitcher: &mut S,
        context: Self::NewContext,
    ) -> Result<(Self, Context), Self::Error>
    where
        Self: Sized,
    {
        let (a_out, b_out) = this;
        let (b, b_ctx) = U::reverse(b_out, stitcher, context)?;
        let (a, a_ctx) = T::reverse(a_out, stitcher, b_ctx)?;
        Ok(((a, b), a_ctx))
    }
}
