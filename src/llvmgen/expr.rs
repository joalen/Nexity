use super::Codegen;
use crate::ast::ast::{BinaryOp, Expr};
use inkwell::IntPredicate;
use inkwell::values::AnyValue;
use inkwell::values::BasicValueEnum;

impl<'ctx> Codegen<'ctx> {
    pub fn compile_expr(&mut self, expr: &Expr) -> Result<BasicValueEnum<'ctx>, String> {
        match expr {
            Expr::Int(n) => Ok(self.context.i64_type().const_int(*n as u64, true).into()),

            Expr::Float(n) => Ok(self.context.f64_type().const_float(*n).into()),

            Expr::Bool(b) => Ok(self.context.bool_type().const_int(*b as u64, false).into()),

            Expr::Identifier(name) => self
                .env
                .get(name)
                .copied()
                .ok_or_else(|| format!("Undefined variable: {}", name)),

            Expr::BinaryOp(lhs, op, rhs) => self.compile_binop(lhs, op, rhs),
            Expr::If(cond, then_e, else_e) => self.compile_if(cond, then_e, else_e),
            Expr::Let(bindings, body) => self.compile_let(bindings, body),
            Expr::Lambda(params, body) => self.compile_lambda(params, body),
            Expr::Application(f, arg) => self.compile_application(f, arg),

            _ => Err(format!("compile_expr: unsupported {:?}", expr)),
        }
    }

    fn compile_binop(
        &mut self,
        lhs: &Expr,
        op: &BinaryOp,
        rhs: &Expr,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let l = self.compile_expr(lhs)?;
        let r = self.compile_expr(rhs)?;

        // int path
        if let (BasicValueEnum::IntValue(lv), BasicValueEnum::IntValue(rv)) = (l, r) {
            return Ok(match op {
                BinaryOp::Add => self.builder.build_int_add(lv, rv, "add").unwrap().into(),
                BinaryOp::Subtract => self.builder.build_int_sub(lv, rv, "sub").unwrap().into(),
                BinaryOp::Multiply => self.builder.build_int_mul(lv, rv, "mul").unwrap().into(),
                BinaryOp::Divide => self
                    .builder
                    .build_int_signed_div(lv, rv, "div")
                    .unwrap()
                    .into(),
                BinaryOp::Equal => self
                    .builder
                    .build_int_compare(IntPredicate::EQ, lv, rv, "eq")
                    .unwrap()
                    .into(),
                BinaryOp::NotEqual => self
                    .builder
                    .build_int_compare(IntPredicate::NE, lv, rv, "ne")
                    .unwrap()
                    .into(),
                BinaryOp::LessThan => self
                    .builder
                    .build_int_compare(IntPredicate::SLT, lv, rv, "lt")
                    .unwrap()
                    .into(),
                BinaryOp::GreaterThan => self
                    .builder
                    .build_int_compare(IntPredicate::SGT, lv, rv, "gt")
                    .unwrap()
                    .into(),
                _ => return Err("Unsupported int op".into()),
            });
        }

        // float path
        if let (BasicValueEnum::FloatValue(lv), BasicValueEnum::FloatValue(rv)) = (l, r) {
            return Ok(match op {
                BinaryOp::Add => self.builder.build_float_add(lv, rv, "fadd").unwrap().into(),
                BinaryOp::Subtract => self.builder.build_float_sub(lv, rv, "fsub").unwrap().into(),
                BinaryOp::Multiply => self.builder.build_float_mul(lv, rv, "fmul").unwrap().into(),
                BinaryOp::Divide => self.builder.build_float_div(lv, rv, "fdiv").unwrap().into(),
                _ => return Err("Unsupported float op".into()),
            });
        }

        Err("Type mismatch in binop".into())
    }

    fn compile_if(
        &mut self,
        cond: &Expr,
        then_e: &Expr,
        else_e: &Expr,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let cond_val = self.compile_expr(cond)?.into_int_value();
        let parent_fn = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();

        let then_bb = self.context.append_basic_block(parent_fn, "then");
        let else_bb = self.context.append_basic_block(parent_fn, "else");
        let merge_bb = self.context.append_basic_block(parent_fn, "merge");

        self.builder
            .build_conditional_branch(cond_val, then_bb, else_bb)
            .unwrap();

        self.builder.position_at_end(then_bb);
        let then_val = self.compile_expr(then_e)?;
        self.builder.build_unconditional_branch(merge_bb).unwrap();
        let then_bb = self.builder.get_insert_block().unwrap(); // may have changed

        self.builder.position_at_end(else_bb);
        let else_val = self.compile_expr(else_e)?;
        self.builder.build_unconditional_branch(merge_bb).unwrap();
        let else_bb = self.builder.get_insert_block().unwrap();

        self.builder.position_at_end(merge_bb);
        let phi = self
            .builder
            .build_phi(self.context.i64_type(), "iftmp")
            .unwrap();
        phi.add_incoming(&[(&then_val, then_bb), (&else_val, else_bb)]);

        Ok(phi.as_basic_value())
    }

    fn compile_let(
        &mut self,
        bindings: &[(String, Expr)],
        body: &Expr,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let mut saved = Vec::new();
        for (name, expr) in bindings {
            let val = self.compile_expr(expr)?;
            let old = self.env.insert(name.clone(), val);
            saved.push((name.clone(), old));
        }
        let result = self.compile_expr(body)?;
        for (name, old) in saved {
            match old {
                Some(v) => {
                    self.env.insert(name, v);
                }
                None => {
                    self.env.remove(&name);
                }
            }
        }
        Ok(result)
    }

    fn compile_lambda(
        &mut self,
        params: &[String],
        body: &Expr,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let i64_t = self.context.i64_type();
        let param_types: Vec<_> = params.iter().map(|_| i64_t.into()).collect();
        let fn_type = i64_t.fn_type(&param_types, false);
        let function = self.module.add_function("lambda", fn_type, None);
        let bb = self.context.append_basic_block(function, "entry");

        // save builder state
        let saved_bb = self.builder.get_insert_block();
        let saved_env = self.env.clone();

        self.builder.position_at_end(bb);
        for (i, param) in params.iter().enumerate() {
            self.env.insert(
                param.clone(),
                function.get_nth_param(i as u32).unwrap().into(),
            );
        }

        let ret_val = self.compile_expr(body)?;
        self.builder.build_return(Some(&ret_val)).unwrap();

        // restore
        self.env = saved_env;
        if let Some(bb) = saved_bb {
            self.builder.position_at_end(bb);
        }

        Ok(function.as_global_value().as_pointer_value().into())
    }

    fn compile_application(
        &mut self,
        func_expr: &Expr,
        arg_expr: &Expr,
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let arg_val = self.compile_expr(arg_expr)?;
        let i64_t = self.context.i64_type();

        if let Expr::Identifier(name) = func_expr {
            if let Some(function) = self.module.get_function(name) {
                let call = self
                    .builder
                    .build_call(function, &[arg_val.into()], "call")
                    .unwrap();
                let val = call
                    .as_any_value_enum()
                    .try_into()
                    .map_err(|_| "call did not return a basic value".to_string())?;
                return Ok(val);
            }
        }

        // fallback: indirect call through pointer (lambdas)
        let func_val = self.compile_expr(func_expr)?.into_pointer_value();
        let fn_type = i64_t.fn_type(&[i64_t.into()], false);
        let call = self
            .builder
            .build_indirect_call(fn_type, func_val, &[arg_val.into()], "call")
            .unwrap();
        let val = call
            .as_any_value_enum()
            .try_into()
            .map_err(|_| "call did not return a basic value".to_string())?;
        Ok(val)
    }
}
