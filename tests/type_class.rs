use std::collections::HashMap;

use nexity::ast::{ast::{BinaryOp, Constructor, Decl, Expr, Type}, types::{ClassDef, Instance, Substitution, TypeDecl, TypeInference, Variant, expand_type_alias}};
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
        &Substitution::new(),
        &type_infer.type_aliases
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
    let list_decl = Decl::Data(
        "List".to_string(),
        vec!["a".to_string()],
        vec![
            Constructor { name: "Nil".to_string(), fields: vec![] },
            Constructor { 
                name: "Cons".to_string(), 
                fields: vec![
                    Type::TypeVar("a".to_string()),
                    Type::Apply(
                        Box::new(Type::Custom("List".to_string())),
                        vec![Type::TypeVar("a".to_string())],
                    ),
                ],
            },
        ]
    );
    type_infer.register_decl(&list_decl);
    
    // Test Nil constructor
    let nil = Expr::Identifier("Nil".to_string());
    let (ty, _) = type_infer.infer(&nil).unwrap();
    assert!(matches!(ty, Type::Apply(..)));
    
    // Test List Int kind checks
    let list_int = Type::Apply(
        Box::new(Type::Custom("List".to_string())),
        vec![Type::Int]
    );
    assert!(type_infer.check_kind(&list_int, &type_infer.kind_env).is_ok());
}
#[test]
fn test_kind_checking()
{
    let mut type_infer = TypeInference::new();

    // test 1 - Register Maybe a = Just a | Nothing
    let maybe_decl = Decl::Data(
        "Maybe".to_string(),
        vec!["a".to_string()],
        vec![
            Constructor { name: "Just".to_string(), fields: vec![Type::TypeVar("a".to_string())] },
            Constructor { name: "Nothing".to_string(), fields: vec![] }
        ]
    );
    type_infer.register_decl(&maybe_decl);

    // test 2 - valid - Maybe Int (kind checks out)
    let valid = Type::Apply(
        Box::new(Type::Custom("Maybe".to_string())),
        vec![Type::Int]
    );
    assert!(type_infer.check_kind(&valid, &type_infer.kind_env).is_ok());
    println!("Maybe Int is valid");

    // test 3 - invalid - Int Int (so we avoid this non-sense)
    let invalid = Type::Apply(
        Box::new(Type::Int),
        vec![Type::Int]
    );
    assert!(type_infer.check_kind(&invalid, &type_infer.kind_env).is_err());
    println!("Int Int rejected!");

    // test 4 - another invalid case - Maybe Int Int (too many type args)
    let too_many = Type::Apply(
        Box::new(Type::Custom("Maybe".to_string())),
        vec![Type::Int, Type::Int]
    );
    assert!(type_infer.check_kind(&too_many, &type_infer.kind_env).is_err());
    println!("Maybe Int Int rejected!");

    // test 5 - Either ab (kind * -> * -> *)
    let either_decl = Decl::Data(
        "Either".to_string(),
        vec!["a".to_string(), "b".to_string()],
        vec![
            Constructor { name: "Left".to_string(), fields: vec![Type::TypeVar("a".to_string())] },
            Constructor { name: "Right".to_string(), fields: vec![Type::TypeVar("b".to_string())] }
        ]
    );
    type_infer.register_decl(&either_decl);

    let either_int_bool = Type::Apply(
        Box::new(Type::Custom("Either".to_string())),
        vec![Type::Int, Type::Bool]
    );
    assert!(type_infer.check_kind(&either_int_bool, &type_infer.kind_env).is_ok());
    println!("Either Int Bool is valid");
}

#[test]
fn test_simple_type_synonym() {
    let mut type_infer = TypeInference::new();
    
    // Register: type MyInt = Int
    type_infer.type_aliases.insert(
        "MyInt".to_string(),
        (vec![], Type::Int)
    );
    
    let expr = Expr::Annotated(
        Box::new(Expr::Int(42)),
        Type::Custom("MyInt".to_string())
    );
    
    let (ty, _) = type_infer.infer(&expr).unwrap();
    assert_eq!(ty, Type::Int);
}

#[test]
fn test_parameterized_type_synonym() {
    let mut type_infer = TypeInference::new();
    
    // Register: type Pair a b = (a, b)
    // For now just test the expansion logic since we don't have tuples
    type_infer.type_aliases.insert(
        "Pair".to_string(),
        (vec!["a".to_string(), "b".to_string()], 
         Type::Function(
             Box::new(Type::TypeVar("a".to_string())),
             Box::new(Type::TypeVar("b".to_string()))
         ))
    );
    
    let pair_int_bool = Type::Apply(
        Box::new(Type::Custom("Pair".to_string())),
        vec![Type::Int, Type::Bool]
    );
    
    let expanded = expand_type_alias(&pair_int_bool, &type_infer.type_aliases);
    assert_eq!(expanded, Type::Function(Box::new(Type::Int), Box::new(Type::Bool)));
}

#[test]
fn test_nested_type_synonym() {
    let mut type_infer = TypeInference::new();
    
    type_infer.type_aliases.insert("MyInt".to_string(), (vec![], Type::Int));
    type_infer.type_aliases.insert("MyMyInt".to_string(), (vec![], Type::Custom("MyInt".to_string())));
    
    let expanded = expand_type_alias(&Type::Custom("MyMyInt".to_string()), &type_infer.type_aliases);
    assert_eq!(expanded, Type::Int);
}

#[test]
fn test_basic_forall()
{ 
    let mut type_infer = TypeInference::new();
    
    // id :: forall a. a -> a
    let id_expr = Expr::Annotated(
        Box::new(Expr::Lambda(
            vec!["x".to_string()],
            Box::new(Expr::Identifier("x".to_string()))
        )),
        Type::Forall(
            vec!["a".to_string()],
            Box::new(Type::Function(
                Box::new(Type::TypeVar("a".to_string())),
                Box::new(Type::TypeVar("a".to_string()))
            ))
        )
    );
    
    match type_infer.infer(&id_expr) {
        Ok((ty, _)) => println!("Basic forall: {:?}", ty),
        Err(e) => println!("Basic forall failed: {}", e),
    }  
}

#[test]
fn test_rank2_application() {
    let mut type_infer = TypeInference::new();
    
    // apply :: (forall a. a -> a) -> Int -> Int
    // apply f x = f x
    let apply_expr = Expr::Lambda(
        vec!["f".to_string(), "x".to_string()],
        Box::new(Expr::Application(
            Box::new(Expr::Identifier("f".to_string())),
            Box::new(Expr::Identifier("x".to_string()))
        ))
    );
    
    match type_infer.infer(&apply_expr) {
        Ok((ty, _)) => println!("Rank-2 application: {:?}", ty),
        Err(e) => println!("Rank-2 application failed: {}", e),
    }
}


#[test]
fn test_runst_style() {
    let mut type_infer = TypeInference::new();
    
    // runST :: (forall s. ST s a) -> a
    // Simplified: \f -> f
    let runst = Expr::Annotated(
        Box::new(Expr::Lambda(
            vec!["f".to_string()],
            Box::new(Expr::Identifier("f".to_string()))
        )),
        Type::Function(
            Box::new(Type::Forall(
                vec!["s".to_string()],
                Box::new(Type::Function(
                    Box::new(Type::TypeVar("s".to_string())),
                    Box::new(Type::TypeVar("a".to_string()))
                ))
            )),
            Box::new(Type::TypeVar("a".to_string()))
        )
    );
    
    match type_infer.infer(&runst) {
        Ok((ty, _)) => println!("runST-style: {:?}", ty),
        Err(e) => println!("runST-style failed: {}", e),
    }
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