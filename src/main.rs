mod lexer;
mod parser;
mod llvmgen;
mod ast; 

use std::collections::HashMap;
use std::vec;

use crate::ast::types::{ClassDef, Instance, TypeInference};
use crate::ast::ast::{BinaryOp, Expr, Type};

fn main() {
    let mut type_infer = TypeInference::new();
    let expr = Expr::Float(42.0);

    // check for a simple number
    match type_infer.infer(&expr) {
        Ok((ty, constraints)) => {
            if constraints.is_empty() {
                println!("Inferred type: {:?}", ty);
            } else {
                println!("Inferred type: {:?} with constraints {:?}", ty, constraints);
            }
        }
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

    let annotated = Expr::Annotated(
        Box::new(Expr::Int(42)),
        Type::Int
    );
    let result = type_infer.infer(&annotated).unwrap();
    println!("Annotated type: {:?}", result);

        // Register Eq class
        type_infer.class_env.classes.insert(
            "Eq".to_string(),
            ClassDef {
                name: "Eq".to_string(),
                param: "a".to_string(),
                methods: HashMap::from([
                    ("==".to_string(), Type::Function(
                        Box::new(Type::TypeVar("a".to_string())),
                        Box::new(Type::Function(
                            Box::new(Type::TypeVar("a".to_string())),
                            Box::new(Type::Bool)
                        ))
                    ))
                ]),
            }
        );
        
        // Register instance Eq Int
        type_infer.class_env.instances.push(Instance {
            class_name: "Eq".to_string(),
            ty: Type::Int,
            methods: HashMap::new(),
        });
        
        // Test: (\x y -> x == y) :: Eq a => a -> a -> Bool
        let eq_lambda = Expr::Lambda(
            vec!["x".to_string(), "y".to_string()],
            Box::new(Expr::BinaryOp(
                Box::new(Expr::Identifier("x".to_string())),
                BinaryOp::Equal,
                Box::new(Expr::Identifier("y".to_string()))
            ))
        );
        
        let result = type_infer.infer(&eq_lambda).unwrap();
        println!("Eq lambda type: {:?}", result);
}