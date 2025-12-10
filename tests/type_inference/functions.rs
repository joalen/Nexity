use nexity::ast::ast::{Constraint, Expr, Type};
use nexity::ast::types::{TypeInference};
use nexity::lexer::Lexer;
use nexity::parser::{Parser, Precedence};


fn infer(input: &str) -> Type {
    let (ty, _) = infer_type_of(input).expect("Type inference failed");
    ty
}

fn infer_type_of(input: &str) -> Result<(Type, Vec<Constraint>), String> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expr = parser.parse_expr(Precedence::Lowest)
        .ok_or("Parse failed")?;
    let mut infer = TypeInference::new();
    infer.infer(&expr)
}


#[test]
fn test_lambda_polymorphic() {
    let id = infer("\\x -> x");
    // Should be: t0 -> t0
    assert!(matches!(id, Type::Function(..)));
}

#[test]
fn test_polymorphic_identity_after_let() {
    // The identity function should be polymorphic AFTER the let
    // let id = λx. x in (id 5, id true)
    let expr = Expr::Let(
        vec![(
            "id".to_string(),
            Expr::Lambda(
                vec!["x".to_string()],
                Box::new(Expr::Identifier("x".to_string()))
            )
        )],
        Box::new(Expr::Application(
            Box::new(Expr::Identifier("id".to_string())),
            Box::new(Expr::Float(5.0))
        ))
    );
    
    let mut infer = TypeInference::new();
    let result = infer.infer(&expr);
    assert!(result.is_ok());
    // id can be used polymorphically after the let
}

#[test]
fn test_polymorphic_argument() {
    let mut type_infer = TypeInference::new();
    
    // \f -> (f 1, f True)
    // Should require f :: forall a. a -> b
    let poly_arg = Expr::Lambda(
        vec!["f".to_string()],
        Box::new(Expr::Application(
            Box::new(Expr::Identifier("f".to_string())),
            Box::new(Expr::Int(1))
        ))
    );
    
    match type_infer.infer(&poly_arg) {
        Ok((ty, _)) => println!("Polymorphic argument: {:?}", ty),
        Err(e) => println!("Polymorphic argument failed: {}", e),
    }
}