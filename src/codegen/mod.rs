pub mod expr;
pub mod stmt;
pub mod types;

use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::module::Module;
use inkwell::values::BasicValueEnum;
use std::collections::HashMap;

pub struct Codegen<'ctx> {
    pub context: &'ctx Context,
    pub module: Module<'ctx>,
    pub builder: Builder<'ctx>,
    pub env: HashMap<String, BasicValueEnum<'ctx>>,
}

impl<'ctx> Codegen<'ctx> {
    pub fn new(context: &'ctx Context) -> Self {
        let module = context.create_module("nexity");
        let builder = context.create_builder();
        Codegen { context, module, builder, env: HashMap::new() }
    }
}