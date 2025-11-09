mod lexer;
mod parser;
mod llvmgen;
mod ast; 

use lexer::{Lexer};
use parser::{Parser, Precedence};
use crate::ast::type_inference::TypeInference;
use crate::ast::ast::{Expr};

fn main() {
    let mut type_infer = TypeInference::new();
    let expr = Expr::Number(42.0);

    // check for a simple number
    match type_infer.infer(&expr) {
        Ok(ty) => println!("Inferred type: {:?}", ty),
        Err(err) => eprintln!("Type inference error: {}", err),
    }

    // identifier 
    let mut env = type_infer.env.clone();
    env.insert("x".into(), ast::ast::Type::Float);
    let expr2 = Expr::Identifier("x".to_string());

    match expr2.infer_type(&mut env, &mut type_infer.type_var_gen) {
        Ok(ty) => println!("Inferred type: {:?}", ty),
        Err(err) => eprintln!("Type inference error: {}", err),
    }
    
}