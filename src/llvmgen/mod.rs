pub mod expr;
pub mod types;

use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::module::Module;
use inkwell::values::BasicValueEnum;
use std::collections::HashMap;
use crate::ast::ast::Expr;

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

    pub fn compile_module(&mut self, expr: &Expr) -> Result<(), String> {
        let i64_t = self.context.i64_type();
        let main_fn = self.module.add_function("main", i64_t.fn_type(&[], false), None);
        let bb = self.context.append_basic_block(main_fn, "entry");
        self.builder.position_at_end(bb);

        let result = self.compile_expr(expr)?;
        self.builder.build_return(Some(&result)).unwrap();
        self.module.verify().map_err(|e| e.to_string())?;
        Ok(())
    }
}