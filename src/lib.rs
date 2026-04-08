pub mod ast;
pub mod lexer;
pub mod parser;
pub mod llvmgen;

use inkwell::context::Context;
use lexer::Lexer;
use parser::{Parser, Precedence};
use ast::types::TypeInference;
use ast::ast::{Decl, Type};
use ast::types::TypeScheme;

use llvmgen::Codegen;

use crate::lexer::Token;

pub fn compile(source: &str) -> Result<String, String> {
    // first, we lex + parse
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);
    let expr = parser.parse_expr(Precedence::Lowest)
        .ok_or("Parse failed")?;

    // then, type check
    let mut ti = TypeInference::new();
    ti.infer(&expr)?;

    // codegen to LLVM IR
    let context = Context::create();
    let mut cg = Codegen::new(&context);
    cg.compile_module(&expr)?;

    Ok(cg.module.print_to_string().to_string())
}

pub fn compile_program(source: &str) -> Result<String, String> {
    // parsing all decls
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);

    let mut decls = Vec::new();
    while parser.current_token != Token::Eof {
        if parser.current_token == Token::VirtualSemi {
            parser.next_token();
            continue;
        }
        match parser.parse_decl() {
            Some(decl) => decls.push(decl),
            None => break,
        }
    }

    // then, type check all decls
    let mut ti = TypeInference::new();
    for decl in &decls {
        if let Decl::FuncDef(name, params, _) = decl {
            let ret_ty = ti.type_var_gen.fresh();
            let ty = params.iter().rfold(ret_ty, |acc, _| {
                let param_ty = ti.type_var_gen.fresh();
                Type::Function(Box::new(param_ty), Box::new(acc))
            });
            ti.env.insert(name.clone(), TypeScheme {
                type_vars: vec![],
                constraints: vec![],
                ty,
            });
        }
    }

    for decl in &decls {
        ti.register_decl(decl);
    }

    // codegeneration3
    let context = Context::create();
    let mut cg = Codegen::new(&context);

    for decl in &decls {
        cg.compile_decl(decl)?;
    }

    Ok(cg.module.print_to_string().to_string())
}