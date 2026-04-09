pub mod ast;
pub mod lexer;
pub mod parser;
pub mod llvmgen;

use inkwell::context::Context;
use inkwell::targets::{
    CodeModel, FileType, InitializationConfig, RelocMode, Target, TargetMachine,
};
use inkwell::OptimizationLevel;

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
            Some(decl) => {
                println!("DECL: {:?}", decl);
                decls.push(decl);
            }
            None => {
                println!("STOPPED AT: {:?}, prev decls: {}", parser.current_token, decls.len());
                break;
            }
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

pub fn compile_to_binary(source: &str, output_path: &str) -> Result<(), String> {
    // parsing
    let lexer = Lexer::new(source);
    let mut parser = Parser::new(lexer);
    let mut decls = Vec::new();
    while parser.current_token != Token::Eof {
        if parser.current_token == Token::VirtualSemi {
            parser.next_token();
            continue;
        }
        match parser.parse_decl() {
            Some(decl) => { 
                println!("DECL: {:?}", decl);
                decls.push(decl);
            }
            None => { 
                println!("PARSE STOPPED AT: {:?}", parser.current_token); 
                break;
            }
        }
    }

    // type checking
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

    // codegen
    let context = Context::create();
    let mut cg = Codegen::new(&context);
    for decl in &decls {
        cg.compile_decl(decl)?;
    }

    // native target init
    Target::initialize_native(&InitializationConfig::default())
        .map_err(|e| e.to_string())?;

    let triple = TargetMachine::get_default_triple();
    let target = Target::from_triple(&triple)
        .map_err(|e| e.to_string())?;
    let machine = target.create_target_machine(
        &triple,
        "generic",
        "",
        OptimizationLevel::Default,
        RelocMode::Default,
        CodeModel::Default,
    ).ok_or("Failed to create target machine")?;

    // write object file
    let obj_path = format!("{}.o", output_path);
    machine.write_to_file(&cg.module, FileType::Object, obj_path.as_ref())
        .map_err(|e| e.to_string())?;

    // system linker linking
    let status = std::process::Command::new("cc")
        .args([&obj_path, "-o", output_path])
        .status()
        .map_err(|e| e.to_string())?;

    if !status.success() {
        return Err("Linking failed".into());
    }

    // remove object file
    std::fs::remove_file(&obj_path).ok();
    Ok(())
}