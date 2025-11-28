use nexity::ast::{ast::{Constructor, Decl, Expr, Type}, types::{TypeDecl, TypeInference, Variant}};

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
                    result_ty: None
                },
                Variant {
                    name: "Just".to_string(),
                    arg_types: vec![Type::TypeVar("a".to_string())],
                    result_ty: None
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
            Constructor { name: "Nil".to_string(), fields: vec![], result_ty: None },
            Constructor { 
                name: "Cons".to_string(), 
                fields: vec![
                    Type::TypeVar("a".to_string()),
                    Type::Apply(
                        Box::new(Type::Custom("List".to_string())),
                        vec![Type::TypeVar("a".to_string())],
                    ),
                ],
                result_ty: None
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