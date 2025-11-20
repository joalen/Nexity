mod lexer;
mod parser;
mod llvmgen;
mod ast; 

use std::vec;

use crate::ast::types::TypeInference;
use crate::ast::ast::{Expr};

fn main() {
    let mut type_infer = TypeInference::new();
    let expr = Expr::Float(42.0);

    // check for a simple number
    match type_infer.infer(&expr) {
        Ok(ty) => println!("Inferred type: {:?}", ty),
        Err(err) => eprintln!("Type inference error: {}", err),
    }

    // identity lambda 
    let id_expr = Expr::Lambda(
        vec!["x".to_string()],
        Box::new(Expr::Identifier("x".to_string()))
    );

    // constant lambda
    let const_expr = Expr::Lambda( 
        vec!["x".to_string(), "y".to_string()],
        Box::new(Expr::Int(42))
    );

    let id_type = type_infer.infer(&id_expr).unwrap();
    let const_type = type_infer.infer(&const_expr).unwrap();

    println!("id_expr type: {:?}", id_type);
    println!("const_expr type: {:?}", const_type);
}