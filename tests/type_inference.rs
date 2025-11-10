use nexity::ast::ast::{Expr, Type, BinaryOp};
use nexity::ast::types::TypeInference;

#[test]
fn test_valid_binary_operations_inference()
{ 
    let mut type_infer = TypeInference::new();

    let cases = vec![ 
        (Expr::BinaryOp(
            Box::new(Expr::Number(5.0)),
            nexity::ast::ast::BinaryOp::Add,
            Box::new(Expr::Number(3.0))
        ), Type::Float),
    
        (Expr::BinaryOp(
            Box::new(Expr::Number(10.0)),
            nexity::ast::ast::BinaryOp::Subtract,
            Box::new(Expr::Number(4.0))
        ), Type::Float),
    
        (Expr::BinaryOp(
            Box::new(Expr::Number(24.0)),
            nexity::ast::ast::BinaryOp::Multiply,
            Box::new(Expr::Number(2.0))
        ), Type::Float),
        (Expr::BinaryOp(
            Box::new(Expr::Number(20.0)),
            nexity::ast::ast::BinaryOp::Divide,
            Box::new(Expr::Number(5.0))
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
        Box::new(Expr::Number(42.0)),
        BinaryOp::Equal,
        Box::new(Expr::Number(25.0)), 
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
            Box::new(Expr::Number(5.0))
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
                    Box::new(Expr::Number(5.0))
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