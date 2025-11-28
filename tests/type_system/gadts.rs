use nexity::ast::{ast::{BinaryOp, Expr, Pattern, Type}, types::{TypeDecl, TypeInference, TypeScheme, Variant}};

#[test]
fn test_basic_gadt() {
    let mut type_infer = TypeInference::new();
    
    // Register GADT: data Expr a where
    //   IntLit :: Int -> Expr Int
    //   BoolLit :: Bool -> Expr Bool
    type_infer.adt_env.insert(
        "Expr".to_string(),
        TypeDecl {
            name: "Expr".to_string(),
            type_params: vec!["a".to_string()],
            variants: vec![
                Variant {
                    name: "IntLit".to_string(),
                    arg_types: vec![Type::Int],
                    result_ty: Some(Type::Apply(
                        Box::new(Type::Custom("Expr".to_string())),
                        vec![Type::Int]
                    )),
                },
                Variant {
                    name: "BoolLit".to_string(),
                    arg_types: vec![Type::Bool],
                    result_ty: Some(Type::Apply(
                        Box::new(Type::Custom("Expr".to_string())),
                        vec![Type::Bool]
                    )),
                },
            ],
        },
    );
    
    // Test: match expr { IntLit n -> n + 1 }
    // Should infer that n :: Int
    let match_expr = Expr::Match(
        Box::new(Expr::Identifier("expr".to_string())),
        vec![(
            Pattern::Constructor("IntLit".to_string(), vec![Pattern::Variable("n".to_string())]),
            None,
            Expr::BinaryOp(
                Box::new(Expr::Identifier("n".to_string())),
                BinaryOp::Add,
                Box::new(Expr::Int(1))
            )
        )]
    );
    
    type_infer.env.insert(
        "expr".to_string(),
        TypeScheme {
            type_vars: vec!["a".to_string()],
            constraints: vec![],
            ty: Type::Apply(
                Box::new(Type::Custom("Expr".to_string())),
                vec![Type::TypeVar("a".to_string())]
            ),
        },
    );
    
    let result = type_infer.infer(&match_expr);
    println!("GADT match result: {:?}", result);
    assert!(result.is_ok());
}

#[test]
fn test_gadt_eval() {
    let mut type_infer = TypeInference::new();
    
    // Same GADT setup
    type_infer.adt_env.insert(
        "Expr".to_string(),
        TypeDecl {
            name: "Expr".to_string(),
            type_params: vec!["a".to_string()],
            variants: vec![
                Variant {
                    name: "IntLit".to_string(),
                    arg_types: vec![Type::Int],
                    result_ty: Some(Type::Apply(
                        Box::new(Type::Custom("Expr".to_string())),
                        vec![Type::Int]
                    )),
                },
                Variant {
                    name: "BoolLit".to_string(),
                    arg_types: vec![Type::Bool],
                    result_ty: Some(Type::Apply(
                        Box::new(Type::Custom("Expr".to_string())),
                        vec![Type::Bool]
                    )),
                },
            ],
        },
    );
    
    // eval :: Expr a -> a
    // eval (IntLit n) = n + 1
    // eval (BoolLit b) = not b
    let eval_expr = Expr::Lambda(
        vec!["e".to_string()],
        Box::new(Expr::Match(
            Box::new(Expr::Identifier("e".to_string())),
            vec![
                (
                    Pattern::Constructor("IntLit".to_string(), vec![Pattern::Variable("n".to_string())]),
                    None,
                    Expr::BinaryOp(
                        Box::new(Expr::Identifier("n".to_string())),
                        BinaryOp::Add,
                        Box::new(Expr::Int(1))
                    )
                ),
                (
                    Pattern::Constructor("BoolLit".to_string(), vec![Pattern::Variable("b".to_string())]),
                    None,
                    Expr::Not(Box::new(Expr::Identifier("b".to_string())))
                ),
            ]
        ))
    );
    
    let result = type_infer.infer(&eval_expr);
    println!("GADT eval type: {:?}", result);
    // Should infer: Expr a -> a
}

#[test]
fn test_gadt_if() {
    let mut type_infer = TypeInference::new();
    
    type_infer.adt_env.insert(
        "Expr".to_string(),
        TypeDecl {
            name: "Expr".to_string(),
            type_params: vec!["a".to_string()],
            variants: vec![
                Variant {
                    name: "IntLit".to_string(),
                    arg_types: vec![Type::Int],
                    result_ty: Some(Type::Apply(
                        Box::new(Type::Custom("Expr".to_string())),
                        vec![Type::Int]
                    )),
                },
                Variant {
                    name: "BoolLit".to_string(),
                    arg_types: vec![Type::Bool],
                    result_ty: Some(Type::Apply(
                        Box::new(Type::Custom("Expr".to_string())),
                        vec![Type::Bool]
                    )),
                },
                Variant {
                    name: "If".to_string(),
                    arg_types: vec![
                        Type::Apply(
                            Box::new(Type::Custom("Expr".to_string())),
                            vec![Type::Bool]
                        ),
                        Type::TypeVar("a".to_string()),
                        Type::TypeVar("a".to_string()),
                    ],
                    result_ty: Some(Type::Apply(
                        Box::new(Type::Custom("Expr".to_string())),
                        vec![Type::TypeVar("a".to_string())]
                    )),
                },
            ],
        },
    );
    
    let result = type_infer.infer(&Expr::Identifier("test".to_string()));
    println!("GADT If constructor test: {:?}", result);
}