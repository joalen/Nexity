use std::collections::HashMap;

use nexity::ast::{ast::{BinaryOp, Expr, Type}, types::{ClassDef, Instance, Substitution, TypeDecl, TypeInference, Variant}};
use nexity::ast::types::solve_constraints;

#[test]
fn test_eq_class_constraint_generation() {
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
    
    let eq_lambda = Expr::Lambda(
        vec!["x".to_string(), "y".to_string()],
        Box::new(Expr::BinaryOp(
            Box::new(Expr::Identifier("x".to_string())),
            BinaryOp::Equal,
            Box::new(Expr::Identifier("y".to_string()))
        ))
    );
    
    let (ty, constraints) = type_infer.infer(&eq_lambda).unwrap();
    
    assert_eq!(constraints.len(), 1);
    assert_eq!(constraints[0].class, "Eq");
    
    println!("Lambda type: {:?}", ty);
    println!("Constraints: {:?}", constraints);
}

#[test]
fn test_constraint_solving() {
    let mut type_infer = TypeInference::new();
    
    type_infer.class_env.classes.insert(
        "Eq".to_string(),
        ClassDef {
            name: "Eq".to_string(),
            param: "a".to_string(),
            methods: HashMap::new(),
        }
    );
    
    type_infer.class_env.instances.push(Instance {
        class_name: "Eq".to_string(),
        ty: Type::Int,
        methods: HashMap::new(),
    });
    
    let eq_ints = Expr::BinaryOp(
        Box::new(Expr::Int(5)),
        BinaryOp::Equal,
        Box::new(Expr::Int(3))
    );
    
    let (ty, constraints) = type_infer.infer(&eq_ints).unwrap();
    
    let unsolved = solve_constraints(
        &constraints, 
        &type_infer.class_env, 
        &Substitution::new()
    ).unwrap();
    
    assert_eq!(ty, Type::Bool);
    assert_eq!(unsolved.len(), 0);
}

#[test]
fn test_maybe_adt() {
    let mut type_infer = TypeInference::new();
    
    // Register Maybe ADT: data Maybe a = Nothing | Just a
    type_infer.adt_env.insert(
        "Maybe".to_string(),
        TypeDecl {
            name: "Maybe".to_string(),
            type_params: vec!["a".to_string()],
            variants: vec![
                Variant {
                    name: "Nothing".to_string(),
                    arg_types: vec![],
                },
                Variant {
                    name: "Just".to_string(),
                    arg_types: vec![Type::TypeVar("a".to_string())],
                },
            ],
        },
    );

    // Test the Nothing constructor
    let nothing = Expr::Identifier("Nothing".to_string());
    let (ty, _) = type_infer.infer(&nothing).unwrap();
    println!("Nothing type: {:?}", ty);

    // Test Just constructor
    let just = Expr::Identifier("Just".to_string());
    let (just_ty, _) = type_infer.infer(&just).unwrap();
    println!("Just type: {:?}", just_ty);
    
    // Test Just 42
    let just_42 = Expr::Application(
        Box::new(Expr::Identifier("Just".to_string())),
        Box::new(Expr::Int(42)),
    );
    let (result_ty, _) = type_infer.infer(&just_42).unwrap();
    println!("Just 42 type: {:?}", result_ty);
}

#[test]
fn test_list_adt() {
    let mut type_infer = TypeInference::new();
    
    // data List a = Nil | Cons a (List a)
    type_infer.adt_env.insert(
        "List".to_string(),
        TypeDecl {
            name: "List".to_string(),
            type_params: vec!["a".to_string()],
            variants: vec![
                Variant {
                    name: "Nil".to_string(),
                    arg_types: vec![],
                },
                Variant {
                    name: "Cons".to_string(),
                    arg_types: vec![
                        Type::TypeVar("a".to_string()),
                        Type::Apply(
                            Box::new(Type::Custom("List".to_string())),
                            vec![Type::TypeVar("a".to_string())],
                        ),
                    ],
                },
            ],
        },
    );

    let nil = Expr::Identifier("Nil".to_string());
    let (ty, _) = type_infer.infer(&nil).unwrap();
    println!("Nil type: {:?}", ty);
}