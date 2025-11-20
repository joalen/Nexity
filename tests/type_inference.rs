use nexity::ast::ast::{Expr, Type, BinaryOp, Pattern};
use nexity::ast::types::{TypeInference};
use nexity::lexer::Lexer;
use nexity::parser::{Parser, Precedence};

// helper method to test out recursive let bindings
fn infer_type_of(input: &str) -> Result<Type, String> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);

    let expr = parser.parse_expr(Precedence::Lowest)
        .ok_or("Parse failed")?;

    let mut infer = TypeInference::new();
    infer.infer(&expr)
}

fn infer(input: &str) -> Type {
    infer_type_of(input).expect("Type inference failed")
}

#[test]
fn test_valid_binary_operations_inference()
{ 
    let mut type_infer = TypeInference::new();

    let cases = vec![ 
        (Expr::BinaryOp(
            Box::new(Expr::Int(5)),
            nexity::ast::ast::BinaryOp::Add,
            Box::new(Expr::Int(3))
        ), Type::Int),
    
        (Expr::BinaryOp(
            Box::new(Expr::Int(1)),
            nexity::ast::ast::BinaryOp::Subtract,
            Box::new(Expr::Int(4))
        ), Type::Int),
    
        (Expr::BinaryOp(
            Box::new(Expr::Float(24.5)),
            nexity::ast::ast::BinaryOp::Multiply,
            Box::new(Expr::Float(2.1111))
        ), Type::Float),
        (Expr::BinaryOp(
            Box::new(Expr::Float(20.0)),
            nexity::ast::ast::BinaryOp::Divide,
            Box::new(Expr::Float(5.0))
        ), Type::Float),
    ];

    for (expr, expected_type) in cases 
    { 
        let inferred_type = type_infer.infer(&expr).unwrap();
        assert_eq!(inferred_type, expected_type);
    }
}

#[test]
fn test_valid_comparison_operations() 
{
    let mut type_infer = TypeInference::new();

    let expr = Expr::BinaryOp(
        Box::new(Expr::Int(42)),
        BinaryOp::Equal,
        Box::new(Expr::Int(25)), 
    );

    let inferred_type = type_infer.infer(&expr).unwrap();
    assert_eq!(inferred_type, Type::Bool);
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
fn test_occurs_check_infinite_type() {
    // This should trigger the occurs check: let f = λx. f
    // f would need type t -> t where t = t -> t (infinite!)
    let expr = Expr::Let(
        vec![(
            "f".to_string(),
            Expr::Lambda(
                vec!["x".to_string()],
                Box::new(Expr::Identifier("f".to_string()))
            )
        )],
        Box::new(Expr::Identifier("f".to_string()))
    );
    
    let mut infer = TypeInference::new();
    let result = infer.infer(&expr);
    // Should fail with occurs check error
    assert!(result.is_err());
    if let Err(e) = result {
        assert!(e.contains("Occurs check") || e.contains("infinite"));
    }
}

#[test]
fn test_self_application_fails() {
    // The classic self-application that can't type: λx. x x
    let expr = Expr::Lambda(
        vec!["x".to_string()],
        Box::new(Expr::Application(
            Box::new(Expr::Identifier("x".to_string())),
            Box::new(Expr::Identifier("x".to_string()))
        ))
    );
    
    let mut infer = TypeInference::new();
    let result = infer.infer(&expr);
    // Should fail: x would need type t -> t' where t = t -> t' (occurs check)
    assert!(result.is_err());
}

#[test]
fn test_fix_point_combinator_fails() {
    // Y combinator: λf. (λx. f (x x)) (λx. f (x x))
    // This can't be typed in simply-typed lambda calculus
    let expr = Expr::Lambda(
        vec!["f".to_string()],
        Box::new(Expr::Application(
            Box::new(Expr::Lambda(
                vec!["x".to_string()],
                Box::new(Expr::Application(
                    Box::new(Expr::Identifier("f".to_string())),
                    Box::new(Expr::Application(
                        Box::new(Expr::Identifier("x".to_string())),
                        Box::new(Expr::Identifier("x".to_string()))
                    ))
                ))
            )),
            Box::new(Expr::Lambda(
                vec!["x".to_string()],
                Box::new(Expr::Application(
                    Box::new(Expr::Identifier("f".to_string())),
                    Box::new(Expr::Application(
                        Box::new(Expr::Identifier("x".to_string())),
                        Box::new(Expr::Identifier("x".to_string()))
                    ))
                ))
            ))
        ))
    );
    
    let mut infer = TypeInference::new();
    let result = infer.infer(&expr);
    assert!(result.is_err());
}

#[test]
fn test_let_generalization_works() {
    // This should work: let id = λx. x in (id 5)
    // Then use id again in a different context
    // The point: id is generalized at the let, so it can be used polymorphically
    let expr = Expr::Let(
        vec![(
            "id".to_string(),
            Expr::Lambda(
                vec!["x".to_string()],
                Box::new(Expr::Identifier("x".to_string()))
            )
        )],
        Box::new(Expr::Let(
            vec![(
                "five".to_string(),
                Expr::Application(
                    Box::new(Expr::Identifier("id".to_string())),
                    Box::new(Expr::Float(5.0))
                )
            )],
            Box::new(Expr::Application(
                Box::new(Expr::Identifier("id".to_string())),
                Box::new(Expr::Bool(true))
            ))
        ))
    );
    
    let mut infer = TypeInference::new();
    let result = infer.infer(&expr);
    assert!(result.is_ok());
}


#[test]
fn test_match_wildcard() 
// test for the wildcard
{ 
    let expr = Expr::Match(
        Box::new(Expr::Bool(true)),
        vec![
            (Pattern::Wildcard, None, Expr::Float(10.0))
        ]
    );

    let mut infer = TypeInference::new();
    let ty = infer.infer(&expr).unwrap();
    assert_eq!(ty, Type::Float);
}

fn test_match_arm_type_mismatch_fails()
    // Should fail: match x { _ => 1, _ => true }
{ 
    let expr = Expr::Match(
        Box::new(Expr::Int(5)),
        vec![
            (Pattern::Wildcard, None, Expr::Int(1)),
            (Pattern::Wildcard, None, Expr::Bool(true)),
        ]
    );

    let mut infer = TypeInference::new();
    let result = infer.infer(&expr);

    assert!(result.is_err(), "Match branches must unify");
}

#[test]
fn test_match_guard_must_be_bool()
// guard is '5' -> not a boolean
{ 
    let expr = Expr::Match(
        Box::new(Expr::Int(1)),
        vec![
            (
                Pattern::Variable("x".into()),
                Some(Expr::Float(5.0)), // NOT boolean
                Expr::Int(0)
            )
        ]
    );

    let mut infer = TypeInference::new();
    let result = infer.infer(&expr);

    assert!(result.is_err());
}

#[test]
fn test_match_generalization_with_identity()
{ 
    let expr = Expr::Match(
        Box::new(Expr::Int(10)),
        vec![(
            Pattern::Variable("id".into()),
            None,
            Expr::Let(
                vec![(
                    "five".into(),
                    Expr::Application(
                        Box::new(Expr::Identifier("id".into())),
                        Box::new(Expr::Float(5.0))
                    )
                )],
                Box::new(Expr::Application(
                    Box::new(Expr::Identifier("id".into())),
                    Box::new(Expr::Bool(true)),
                ))
            )
        )]
    );

    let mut infer = TypeInference::new();
    let result = infer.infer(&expr);
    assert!(result.is_ok());
}

#[test]
fn test_reject_polymorphic_recursion() {
    let code = r#"
        let f = \x -> (f [x], f x)
    "#;

    assert!(infer_type_of(code).is_err());
}

#[test]
fn test_int_float_literals() {
    assert_eq!(infer("42"), Type::Int);
    assert_eq!(infer("42.0"), Type::Float);
}

#[test]
fn test_lambda_polymorphic() {
    let id = infer("\\x -> x");
    // Should be: t0 -> t0
    assert!(matches!(id, Type::Function(..)));
}

#[test]
fn test_match_expression() {
    let expr = "match 42 { 0 -> False, _ -> True }";
    assert_eq!(infer(expr), Type::Bool);
}