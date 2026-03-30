#![no_std]

use portal_pc_waffle_ir::Module;
extern crate alloc;
pub struct RuntimeModule<'a>{
    module: Module<'a>
}
impl<'a> RuntimeModule<'a>{
    pub fn try_new(module: Module<'a>) -> Option<Self>{
        Some(Self{module})
    }
}