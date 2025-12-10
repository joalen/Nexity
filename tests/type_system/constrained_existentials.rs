use nexity::ast::ast::{Constraint, Constructor, Decl, Expr, Pattern, Type};
use nexity::ast::types::{TypeInference, ClassDef, Instance};
use std::collections::HashMap;

#[test]
fn test_parse_existential_with_constraint() {
    use nexity::lexer::Lexer;
    use nexity::parser::Parser;
    
    let input = "exists a. Eq a => a -> ShowBox";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    
    let ty = parser.parse_type().unwrap();
    
    match ty {
        Type::Existential(vars, constraints, _body) => {
            assert_eq!(vars, vec!["a".to_string()]);
            assert_eq!(constraints.len(), 1);
            assert_eq!(constraints[0].class, "Eq");
            // Check body is a -> ShowBox
        }
        _ => panic!("Expected existential type"),
    }
}

#[test]
fn test_showbox_gadt_with_constraint() {
    let mut type_infer = TypeInference::new();
    
    type_infer.class_env.classes.insert(
        "Show".to_string(),
        ClassDef {
            name: "Show".to_string(),
            param: "a".to_string(),
            methods: HashMap::from([
                ("show".to_string(), Type::Function(
                    Box::new(Type::TypeVar("a".to_string())),
                    Box::new(Type::Custom("String".to_string()))
                ))
            ]),
        }
    );
    
    type_infer.class_env.instances.push(Instance {
        class_name: "Show".to_string(),
        ty: Type::Int,
        methods: HashMap::new(),
    });
    
    // data ShowBox where
    //   MkBox :: exists a. Show a => a -> ShowBox
    type_infer.register_decl(&Decl::Data(
        "ShowBox".to_string(),
        vec![],
        vec![Constructor {
            name: "MkBox".to_string(),
            fields: vec![Type::TypeVar("a".to_string())],
            result_ty: Some(Type::Custom("ShowBox".to_string())),
            existential_vars: vec!["a".to_string()],
            existential_constraints: vec![Constraint {
                class: "Show".to_string(),
                ty: Box::new(Type::TypeVar("a".to_string())),
            }],
        }],
    ));
    
    // Test: MkBox 42
    // Should infer as ShowBox with constraint Show Int
    let expr = Expr::Application(
        Box::new(Expr::Identifier("MkBox".to_string())),
        Box::new(Expr::Int(42))
    );
    
    let result = type_infer.infer(&expr);
    assert!(result.is_ok());
    
    let (ty, constraints) = result.unwrap();
    assert_eq!(ty, Type::Custom("ShowBox".to_string()));
    
    // Should have Show Int constraint
    assert!(constraints.iter().any(|c| 
        c.class == "Show" && matches!(&*c.ty, Type::Int)
    ));
}

#[test]
fn test_pattern_match_extracts_constraint() {
    let mut type_infer = TypeInference::new();
    
    // Setup Show class
    type_infer.class_env.classes.insert(
        "Show".to_string(),
        ClassDef {
            name: "Show".to_string(),
            param: "a".to_string(),
            methods: HashMap::from([
                ("show".to_string(), Type::Function(
                    Box::new(Type::TypeVar("a".to_string())),
                    Box::new(Type::Custom("String".to_string()))
                ))
            ]),
        }
    );
    
    type_infer.class_env.instances.push(Instance {
        class_name: "Show".to_string(),
        ty: Type::Int,
        methods: HashMap::new(),
    });
    
    // data ShowBox where
    //   MkBox :: exists a. Show a => a -> ShowBox
    type_infer.register_decl(&Decl::Data(
        "ShowBox".to_string(),
        vec![],
        vec![Constructor {
            name: "MkBox".to_string(),
            fields: vec![Type::TypeVar("a".to_string())],
            result_ty: Some(Type::Custom("ShowBox".to_string())),
            existential_vars: vec!["a".to_string()],
            existential_constraints: vec![Constraint {
                class: "Show".to_string(),
                ty: Box::new(Type::TypeVar("a".to_string())),
            }],
        }],
    ));
    
    // Test: match box { MkBox x => x }
    // The type of x should be rigid with Show constraint
    let box_var = Expr::Identifier("box".to_string());
    
    let match_expr = Expr::Match(
        Box::new(box_var),
        vec![(
            Pattern::Constructor(
                "MkBox".to_string(),
                vec![Pattern::Variable("x".to_string())]
            ),
            None,
            Expr::Identifier("x".to_string())
        )]
    );
    
    // Set up box : ShowBox in env
    type_infer.env.insert(
        "box".to_string(),
        nexity::ast::types::TypeScheme {
            type_vars: vec![],
            constraints: vec![],
            ty: Type::Custom("ShowBox".to_string()),
        }
    );
    
    let result = type_infer.infer(&match_expr);
    assert!(result.is_ok());
    
    let (ty, constraints) = result.unwrap();
    
    // The type should be rigid (existential)
    match ty {
        Type::Rigid(_) => {
            // Good! It's opaque
        }
        _ => panic!("Expected rigid type for existential variable, got {:?}", ty),
    }
    
    // Should have Show constraint on the rigid type
    assert_eq!(constraints.len(), 1);
    assert_eq!(constraints[0].class, "Show");
}

#[test]
fn test_eq_box_gadt() {
    let mut type_infer = TypeInference::new();
    
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
    
    type_infer.class_env.instances.push(Instance {
        class_name: "Eq".to_string(),
        ty: Type::Int,
        methods: HashMap::new(),
    });
    
    // data EqBox where
    //   MkEqBox :: exists a. Eq a => a -> a -> EqBox
    type_infer.register_decl(&Decl::Data(
        "EqBox".to_string(),
        vec![],
        vec![Constructor {
            name: "MkEqBox".to_string(),
            fields: vec![
                Type::TypeVar("a".to_string()),
                Type::TypeVar("a".to_string())
            ],
            result_ty: Some(Type::Custom("EqBox".to_string())),
            existential_vars: vec!["a".to_string()],
            existential_constraints: vec![Constraint {
                class: "Eq".to_string(),
                ty: Box::new(Type::TypeVar("a".to_string())),
            }],
        }],
    ));
    
    // Test: MkEqBox 5 10
    let expr = Expr::Application(
        Box::new(Expr::Application(
            Box::new(Expr::Identifier("MkEqBox".to_string())),
            Box::new(Expr::Int(5))
        )),
        Box::new(Expr::Int(10))
    );
    
    let result = type_infer.infer(&expr);
    assert!(result.is_ok());
    
    let (ty, constraints) = result.unwrap();
    assert_eq!(ty, Type::Custom("EqBox".to_string()));
    
    // Should have Eq Int constraint
    assert!(constraints.iter().any(|c| 
        c.class == "Eq" && matches!(&*c.ty, Type::Int)
    ));
}

#[test]
fn test_multiple_constraints() {
    let mut type_infer = TypeInference::new();
    
    // Register Show + Eq classes
    type_infer.class_env.classes.insert(
        "Show".to_string(),
        ClassDef {
            name: "Show".to_string(),
            param: "a".to_string(),
            methods: HashMap::new(),
        }
    );
    
    type_infer.class_env.classes.insert(
        "Eq".to_string(),
        ClassDef {
            name: "Eq".to_string(),
            param: "a".to_string(),
            methods: HashMap::new(),
        }
    );
    
    // data MultiBox where
    //   MkMulti :: exists a. (Show a, Eq a) => a -> MultiBox
    // For now, test with single constraint (parser doesn't handle tuples yet)
    type_infer.register_decl(&Decl::Data(
        "MultiBox".to_string(),
        vec![],
        vec![Constructor {
            name: "MkMulti".to_string(),
            fields: vec![Type::TypeVar("a".to_string())],
            result_ty: Some(Type::Custom("MultiBox".to_string())),
            existential_vars: vec!["a".to_string()],
            existential_constraints: vec![
                Constraint {
                    class: "Show".to_string(),
                    ty: Box::new(Type::TypeVar("a".to_string())),
                },
                Constraint {
                    class: "Eq".to_string(),
                    ty: Box::new(Type::TypeVar("a".to_string())),
                }
            ],
        }],
    ));
    
    type_infer.class_env.instances.push(Instance {
        class_name: "Show".to_string(),
        ty: Type::Int,
        methods: HashMap::new(),
    });
    
    type_infer.class_env.instances.push(Instance {
        class_name: "Eq".to_string(),
        ty: Type::Int,
        methods: HashMap::new(),
    });
    
    // Test: MkMulti 42
    let expr = Expr::Application(
        Box::new(Expr::Identifier("MkMulti".to_string())),
        Box::new(Expr::Int(42))
    );
    
    let result = type_infer.infer(&expr);
    assert!(result.is_ok());
    
    let (ty, constraints) = result.unwrap();
    assert_eq!(ty, Type::Custom("MultiBox".to_string()));
    
    // Should have BOTH constraints
    assert!(constraints.iter().any(|c| c.class == "Show"));
    assert!(constraints.iter().any(|c| c.class == "Eq"));
}