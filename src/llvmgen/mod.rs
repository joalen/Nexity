#![allow(dead_code)]

pub mod expr;
pub mod types;

use inkwell::context::Context;
use inkwell::builder::Builder;
use inkwell::module::Module;
use inkwell::values::BasicValueEnum;
use std::collections::HashMap;

use crate::ast::ast::Expr;
use crate::ast::ast::Decl;

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

    pub fn compile_decl(&mut self, decl: &Decl) -> Result<(), String> {
        match decl {
            Decl::FuncDef(name, params, body) => {
                let i64_t = self.context.i64_type();
                let param_types: Vec<_> = params.iter().map(|_| i64_t.into()).collect();
                let fn_type = i64_t.fn_type(&param_types, false);
                let function = self.module.add_function(name, fn_type, None);
                let bb = self.context.append_basic_block(function, "entry");

                let saved_env = self.env.clone();
                self.builder.position_at_end(bb);

                for (i, param) in params.iter().enumerate() {
                    let val = function.get_nth_param(i as u32).unwrap();
                    val.set_name(param);
                    self.env.insert(param.clone(), val.into());
                }

                let ret = self.compile_expr(body)?;
                self.builder.build_return(Some(&ret)).unwrap();
                self.env = saved_env;
                Ok(())
            }

            // rapid prototyping... so skip type sigs, aliases, data decls for now
            _ => Ok(())
        }
    }
}