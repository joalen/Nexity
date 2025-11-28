use nexity::ast::{ast::{Expr, Pattern}, types::TypeInference};

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
fn test_match_generalization_with_identity()
{ 
    let expr = Expr::Match(
        Box::new(Expr::Lambda(vec!["x".into()], Box::new(Expr::Identifier("x".into())))), // Identity function
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