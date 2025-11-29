use nexity::{ast::{ast::{BinaryOp, Constructor, Decl, Expr, Pattern, Type}, types::TypeInference}, lexer::Lexer, parser::Parser};

#[test]
fn test_basic_existential_parsing() {
    let input = "exists a. a -> Int";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let ty = parser.parse_type().unwrap();
    
    assert_eq!(
        ty,
        Type::Existential(
            vec!["a".to_string()],
            vec![],
            Box::new(Type::Function(
                Box::new(Type::TypeVar("a".to_string())),
                Box::new(Type::Int)
            ))
        )
    );
}

#[test]
fn test_existential_box_data() {
    let input = "data Box where { MkBox :: exists a. a -> Box }";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    
    // Parse the data declaration
    let decl = parser.parse_data();
    assert!(decl.is_some());
    
    if let Some(Decl::Data(name, type_params, constructors)) = decl {
        assert_eq!(name, "Box");
        assert_eq!(type_params.len(), 0);
        assert_eq!(constructors.len(), 1);
        
        let ctor = &constructors[0];
        assert_eq!(ctor.name, "MkBox");
        assert_eq!(ctor.existential_vars, vec!["a".to_string()]);
        assert_eq!(ctor.fields.len(), 1);
    }
}

#[test]
fn test_existential_type_hiding() {
    let mut type_infer = TypeInference::new();
    
    // Register Box ADT with existential constructor
    let box_decl = Decl::Data(
        "Box".to_string(),
        vec![],
        vec![Constructor {
            name: "MkBox".to_string(),
            fields: vec![Type::TypeVar("a".to_string())],
            result_ty: Some(Type::Custom("Box".to_string())),
            existential_vars: vec!["a".to_string()],
            existential_constraints: vec![]
        }]
    );
    type_infer.register_decl(&box_decl);
    
    // MkBox 42 should have type Box
    let int_box = Expr::Application(
        Box::new(Expr::Identifier("MkBox".to_string())),
        Box::new(Expr::Int(42))
    );
    
    // MkBox "hello" should also have type Box
    let str_box = Expr::Application(
        Box::new(Expr::Identifier("MkBox".to_string())),
        Box::new(Expr::String("hello".to_string()))
    );
    
    let (ty1, _) = type_infer.infer(&int_box).unwrap();
    let (ty2, _) = type_infer.infer(&str_box).unwrap();
    
    // Both should be Box (existential hides the actual type)
    assert_eq!(ty1, Type::Custom("Box".to_string()));
    assert_eq!(ty2, Type::Custom("Box".to_string()));
}

#[test]
fn test_existential_unpacking_pattern_match() {
    let mut type_infer = TypeInference::new();
    
    // Register Box ADT
    let box_decl = Decl::Data(
        "Box".to_string(),
        vec![],
        vec![Constructor {
            name: "MkBox".to_string(),
            fields: vec![Type::TypeVar("a".to_string())],
            result_ty: Some(Type::Custom("Box".to_string())),
            existential_vars: vec!["a".to_string()],
            existential_constraints: vec![]
        }]
    );
    type_infer.register_decl(&box_decl);
    
    // match MkBox 42 { MkBox x -> x }
    let expr = Expr::Match(
        Box::new(Expr::Application(
            Box::new(Expr::Identifier("MkBox".to_string())),
            Box::new(Expr::Int(42))
        )),
        vec![(
            Pattern::Constructor("MkBox".to_string(), vec![Pattern::Variable("x".to_string())]),
            None,
            Expr::Identifier("x".to_string())
        )]
    );
    
    let (result_ty, _) = type_infer.infer(&expr).unwrap();
    
    assert!(matches!(result_ty, Type::Rigid(_)));  // ← CHANGE THIS
}

#[test]
fn test_multiple_existential_vars() {
    let input = "exists a b. a -> b -> Int";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let ty = parser.parse_type().unwrap();
    
    assert_eq!(
        ty,
        Type::Existential(
            vec!["a".to_string(), "b".to_string()],
            vec![],
            Box::new(Type::Function(
                Box::new(Type::TypeVar("a".to_string())),
                Box::new(Type::Function(
                    Box::new(Type::TypeVar("b".to_string())),
                    Box::new(Type::Int)
                ))
            ))
        )
    );
}

#[test]
fn test_nested_existential_and_forall() {
    let input = "forall a. exists b. a -> b -> a";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let ty = parser.parse_type().unwrap();
    
    assert_eq!(
        ty,
        Type::Forall(
            vec!["a".to_string()],
            Box::new(Type::Existential(
                vec!["b".to_string()],
                vec![],
                Box::new(Type::Function(
                    Box::new(Type::TypeVar("a".to_string())),
                    Box::new(Type::Function(
                        Box::new(Type::TypeVar("b".to_string())),
                        Box::new(Type::TypeVar("a".to_string()))
                    ))
                ))
            ))
        )
    );
}

#[test]
fn test_existential_prevents_type_assumption() {
    let mut type_infer = TypeInference::new();
    
    // Register Box ADT
    let box_decl = Decl::Data(
        "Box".to_string(),
        vec![],
        vec![Constructor {
            name: "MkBox".to_string(),
            fields: vec![Type::TypeVar("a".to_string())],
            result_ty: Some(Type::Custom("Box".to_string())),
            existential_vars: vec!["a".to_string()],
            existential_constraints: vec![]
        }]
    );
    type_infer.register_decl(&box_decl);
    
    // This should FAIL: match MkBox 42 { MkBox x -> x + 1 }
    // We can't assume x is an Int even though we put 42 in
    let expr = Expr::Match(
        Box::new(Expr::Application(
            Box::new(Expr::Identifier("MkBox".to_string())),
            Box::new(Expr::Int(42))
        )),
        vec![(
            Pattern::Constructor("MkBox".to_string(), vec![Pattern::Variable("x".to_string())]),
            None,
            Expr::BinaryOp(
                Box::new(Expr::Identifier("x".to_string())),
                BinaryOp::Add,
                Box::new(Expr::Int(1))
            )
        )]
    );
    
    // Should fail type checking
    let result = type_infer.infer(&expr);
    assert!(result.is_err());
}