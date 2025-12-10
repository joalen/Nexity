use nexity::ast::{ast::Expr, types::TypeInference};

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