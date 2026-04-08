pub mod ast;
pub mod lexer;
pub mod parser;
pub mod llvmgen;

use inkwell::context::Context;
use lexer::Lexer;
use parser::{Parser, Precedence};
use ast::types::TypeInference;
use llvmgen::Codegen;

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