use std::collections::HashMap;
use crate::ast::{Expr, BinaryOp, Stmt};

pub struct LLVMGenerator {
    code: Vec<String>,
    register_counter: u32,
    env: HashMap<String, String>,
}

impl LLVMGenerator {
    pub fn new() -> Self {
        LLVMGenerator {
            code: Vec::new(),
            register_counter: 0,
            env: HashMap::new(),
        }
    }

    fn new_register(&mut self) -> String {
        let reg = format!("%r{}", self.register_counter);
        self.register_counter += 1;
        reg
    }

    pub fn generate_expr(&mut self, expr: &Expr) -> String {
        match expr {
            Expr::Number(n) => format!("{}", n),

            Expr::BinaryOp(lhs, op, rhs) => {
                let lhs_code = self.generate_expr(lhs);
                let rhs_code = self.generate_expr(rhs);
                let op_code = match op {
                    BinaryOp::Add => "add",
                    BinaryOp::Subtract => "sub",
                    BinaryOp::Multiply => "mul",
                    BinaryOp::Divide => "sdiv",
                    BinaryOp::Modulo => "srem",
                    BinaryOp::And => "and",
                    BinaryOp::Or => "or",
                    BinaryOp::Equal => "icmp eq",
                    BinaryOp::NotEqual => "icmp ne",
                    BinaryOp::LessThan => "icmp slt",
                    BinaryOp::GreaterThan => "icmp sgt",
                    BinaryOp::LessEqual => "icmp sle",
                    BinaryOp::GreaterEqual => "icmp sge",
                };
                let reg = self.new_register();
                self.code.push(format!("{} = {} i32 {}, {}", reg, op_code, lhs_code, rhs_code));
                reg
            }

            Expr::And(lhs, rhs) => {
                let lhs_code = self.generate_expr(lhs);
                let rhs_code = self.generate_expr(rhs);
                let reg = self.new_register();
                self.code.push(format!("{} = and i32 {}, {}", reg, lhs_code, rhs_code));
                reg
            }

            Expr::Or(lhs, rhs) => {
                let lhs_code = self.generate_expr(lhs);
                let rhs_code = self.generate_expr(rhs);
                let reg = self.new_register();
                self.code.push(format!("{} = or i32 {}, {}", reg, lhs_code, rhs_code));
                reg
            }

            Expr::Identifier(name) => {
                if let Some(llvm_name) = self.env.get(name) {
                    llvm_name.clone()
                } else {
                    panic!("Undefined variable '{}'", name);
                }
            }

            Expr::Let(bindings, body) => {
                let mut local_env = self.env.clone();
                for (name, expr) in bindings {
                    let llvm_value = self.generate_expr(expr);
                    let llvm_name = self.new_register();
                    local_env.insert(name.clone(), llvm_name.clone());
                    self.code.push(format!("{} = alloca i32", llvm_name));
                    self.code.push(format!("store i32 {}, i32* {}", llvm_value, llvm_name));
                }
                self.generate_expr(body)
            }

            Expr::While(cond, body) => {
                let cond_label = self.new_register();
                let body_label = self.new_register();
                let end_label = self.new_register();

                self.code.push(format!("br label {}", cond_label));
                self.code.push(format!("{}:", cond_label));
                let cond_code = self.generate_expr(cond);
                self.code.push(format!("br i1 {}, label {}, label {}", cond_code, body_label, end_label));

                self.code.push(format!("{}:", body_label));
                let body_code = self.generate_expr(body);
                self.code.push(format!("br label {}", cond_label));

                self.code.push(format!("{}:", end_label));
                body_code
            }

            Expr::Function(name, body) => {
                let param_str = format!("i32 {}", name);
                let signature = format!("define i32 @{}({})", name, param_str);
                let body_code = self.generate_expr(body);
                self.code.push(format!("{}\n{{\n{}\n}}", signature, body_code));
                String::from(name)
            }

            Expr::Call(func, args) => {
                let func_code = self.generate_expr(func);
                let args_code: Vec<String> = args.iter().map(|arg| self.generate_expr(arg)).collect();
                let reg = self.new_register();
                self.code.push(format!("{} = call i32 {}({})", reg, func_code, args_code.join(", ")));
                reg
            }

            Expr::Return(expr) => {
                let ret_code = self.generate_expr(expr);
                self.code.push(format!("ret i32 {}", ret_code));
                ret_code
            }

            Expr::Lambda(params, body) => {
                let param_strs: Vec<String> = params.iter().map(|p| format!("i32 {}", p)).collect();
                let body_code = self.generate_expr(body);
                let reg = self.new_register();
                self.code.push(format!(
                    "{} = define i32 @lambda({}) {{\n{}\n}}",
                    reg,
                    param_strs.join(", "),
                    body_code
                ));
                reg
            }

            Expr::Application(func_expr, arg_expr) => {
                let func_code = self.generate_expr(func_expr);
                let arg_code = self.generate_expr(arg_expr);
                let reg = self.new_register();
                self.code.push(format!("{} = call i32 {}({})", reg, func_code, arg_code));
                reg
            }

            Expr::Pipe(func_expr, arg_expr) => {
                let func_code = self.generate_expr(func_expr);
                let arg_code = self.generate_expr(arg_expr);
                let reg = self.new_register();
                self.code.push(format!("{} = call i32 {}({})", reg, func_code, arg_code));
                reg
            }

            Expr::If(cond, then_expr, else_expr) => {
                let cond_code = self.generate_expr(cond);
                let then_code = self.generate_expr(then_expr);
                let else_code = self.generate_expr(else_expr);

                let reg = self.new_register();
                self.code.push(format!("{} = icmp ne i32 {}, 0", reg, cond_code));
                let then_block = format!("br i1 {}, label %then, label %else", reg);
                self.code.push(then_block);

                self.code.push(format!("%then:\n{}", then_code));
                self.code.push(format!("%else:\n{}", else_code));

                reg
            }
            _ => unimplemented!(),
        }
    }

    pub fn generate_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Expr(expr) => {
                self.generate_expr(expr);
            }

            Stmt::Let(name, expr) => {
                let value_code = self.generate_expr(expr);
                let llvm_name = self.new_register();
                self.env.insert(name.clone(), llvm_name.clone());
                self.code.push(format!("{} = alloca i32", llvm_name));
                self.code.push(format!("store i32 {}, i32* {}", value_code, llvm_name));
            }

            Stmt::Print(expr) => {
                let value_code = self.generate_expr(expr);
                self.code.push(format!(
                    "call void @print(i32 {})",
                    value_code
                ));
            }

            Stmt::Return(expr) => {
                let return_code = self.generate_expr(expr);
                self.code.push(format!("ret i32 {}", return_code));
            }
        }
    }

    pub fn generate_code(&mut self, stmts: &[Stmt]) -> String {
        self.code.clear();
        for stmt in stmts {
            self.generate_stmt(stmt);
        }
        self.code.join("\n")
    }
}
