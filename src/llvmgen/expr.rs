#![allow(dead_code)]

use super::Codegen;
use crate::ast::ast::{BinaryOp, Expr, Pattern};
use inkwell::IntPredicate;
use inkwell::values::AnyValue;
use inkwell::values::BasicMetadataValueEnum;
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
            Expr::Match(scrutinee, arms) => self.compile_match(scrutinee, arms),

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
        // flatten nested apps into (func_name)
        let mut args = vec![arg_expr];
        let mut head = func_expr;
        while let Expr::Application(f, a) = head {
            args.push(a);
            head = f;
        }
        args.reverse();

        let compiled_args: Vec<BasicMetadataValueEnum> = args
            .iter()
            .map(|a| {
                self.compile_expr(a)
                    .map(|v| BasicMetadataValueEnum::from(v))
            })
            .collect::<Result<_, _>>()?;

        // builtin print
        if let Expr::Identifier(name) = head {
            if name == "print" {
                let val = BasicValueEnum::try_from(compiled_args[0])
                    .map_err(|_| "print: invalid argument type".to_string())?;
                self.emit_printf(val)?;
                return Ok(self.context.i64_type().const_int(0, false).into());
            }
        }

        // direct named call
        if let Expr::Identifier(name) = head {
            if let Some(function) = self.module.get_function(name) {
                let call = self
                    .builder
                    .build_call(function, &compiled_args, "call")
                    .unwrap();
                let val = call
                    .as_any_value_enum()
                    .try_into()
                    .map_err(|_| "call did not return a basic value".to_string())?;
                return Ok(val);
            }
        }

        // indirect calls for lambdab-based expressions
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

    pub fn emit_printf(&mut self, val: BasicValueEnum<'ctx>) -> Result<(), String> {
        let i32_t = self.context.i32_type();
        let ptr_t = self.context.ptr_type(inkwell::AddressSpace::default());

        let printf = self.module.get_function("printf").unwrap_or_else(|| {
            let printf_ty = i32_t.fn_type(&[ptr_t.into()], true);
            self.module.add_function("printf", printf_ty, None)
        });

        let (fmt_str, print_val) = match val {
            BasicValueEnum::IntValue(v) => {
                // check if it's bool (i1) vs int (i64)
                if v.get_type().get_bit_width() == 1 {
                    let fmt = self.builder.build_global_string_ptr("%s\n", "fmt").unwrap();
                    // convert bool to "true"/"false" string
                    let true_str = self
                        .builder
                        .build_global_string_ptr("true", "true_str")
                        .unwrap();
                    let false_str = self
                        .builder
                        .build_global_string_ptr("false", "false_str")
                        .unwrap();
                    let selected = self
                        .builder
                        .build_select(
                            v,
                            true_str.as_pointer_value(),
                            false_str.as_pointer_value(),
                            "bool_str",
                        )
                        .unwrap();
                    (fmt.as_pointer_value().into(), selected.into())
                } else {
                    let fmt = self
                        .builder
                        .build_global_string_ptr("%lld\n", "fmt")
                        .unwrap();
                    (fmt.as_pointer_value().into(), val)
                }
            }
            BasicValueEnum::FloatValue(_) => {
                let fmt = self.builder.build_global_string_ptr("%f\n", "fmt").unwrap();
                (fmt.as_pointer_value().into(), val)
            }
            _ => return Err("print: unsupported type".into()),
        };

        self.builder
            .build_call(printf, &[fmt_str, print_val.into()], "printf_call")
            .unwrap();

        Ok(())
    }

    fn compile_match(
        &mut self,
        scrutinee: &Expr,
        arms: &[(Pattern, Option<Expr>, Expr)],
    ) -> Result<BasicValueEnum<'ctx>, String> {
        let val = self.compile_expr(scrutinee)?;
        let parent_fn = self
            .builder
            .get_insert_block()
            .unwrap()
            .get_parent()
            .unwrap();
        let merge_bb = self.context.append_basic_block(parent_fn, "match_merge");

        let mut phi_incoming: Vec<(BasicValueEnum<'ctx>, inkwell::basic_block::BasicBlock)> =
            Vec::new();

        for (i, (pattern, guard, body)) in arms.iter().enumerate() {
            let check_bb = self
                .context
                .append_basic_block(parent_fn, &format!("arm{}_check", i));
            let body_bb = self
                .context
                .append_basic_block(parent_fn, &format!("arm{}_body", i));
            let next_bb = self
                .context
                .append_basic_block(parent_fn, &format!("arm{}_next", i));

            self.builder.build_unconditional_branch(check_bb).unwrap();
            self.builder.position_at_end(check_bb);

            // emit pattern check — returns i1
            let matched = self.compile_pattern_check(val, pattern)?;

            // bind variables from pattern into env
            let saved_env = self.env.clone();
            self.compile_pattern_bindings(val, pattern)?;

            // optional guard
            let final_check = if let Some(guard_expr) = guard {
                let guard_val = self.compile_expr(guard_expr)?.into_int_value();
                let cond = self
                    .builder
                    .build_and(matched, guard_val, "guarded")
                    .unwrap();
                cond
            } else {
                matched
            };

            self.builder
                .build_conditional_branch(final_check, body_bb, next_bb)
                .unwrap();

            // body
            self.builder.position_at_end(body_bb);
            let result = self.compile_expr(body)?;
            self.builder.build_unconditional_branch(merge_bb).unwrap();
            let body_end_bb = self.builder.get_insert_block().unwrap();
            phi_incoming.push((result, body_end_bb));

            self.env = saved_env;
            self.builder.position_at_end(next_bb);
        }

        // no match — unreachable (type checker guarantees exhaustiveness)
        self.builder.build_unreachable().unwrap();

        self.builder.position_at_end(merge_bb);
        let phi = self
            .builder
            .build_phi(self.context.i64_type(), "match_result")
            .unwrap();
        for (val, bb) in &phi_incoming {
            phi.add_incoming(&[(val, *bb)]);
        }

        Ok(phi.as_basic_value())
    }

    fn compile_pattern_check(
        &mut self,
        val: BasicValueEnum<'ctx>,
        pattern: &Pattern,
    ) -> Result<inkwell::values::IntValue<'ctx>, String> {
        match pattern {
            Pattern::Wildcard | Pattern::Variable(_) => {
                // always matches
                Ok(self.context.bool_type().const_int(1, false))
            }

            Pattern::Literal(n) => {
                let lhs = val.into_int_value();
                let rhs = self.context.i64_type().const_int(*n as u64, true);
                Ok(self
                    .builder
                    .build_int_compare(IntPredicate::EQ, lhs, rhs, "lit_cmp")
                    .unwrap())
            }

            Pattern::Constructor(_, _) => {
                // ADT matching — leave for next PR
                Err("Constructor patterns not yet supported in codegen".into())
            }
        }
    }

    fn compile_pattern_bindings(
        &mut self,
        val: BasicValueEnum<'ctx>,
        pattern: &Pattern,
    ) -> Result<(), String> {
        match pattern {
            Pattern::Variable(name) => {
                self.env.insert(name.clone(), val);
                Ok(())
            }
            Pattern::Wildcard | Pattern::Literal(_) => Ok(()),
            Pattern::Constructor(_, _) => Ok(()), // later
        }
    }
}
