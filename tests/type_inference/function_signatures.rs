use nexity::ast::ast::{Decl, Expr, Type};
use nexity::ast::types::TypeInference;
use nexity::lexer::Lexer;
use nexity::parser::Parser;

#[test]
fn test_parse_type_signature() {
    let input = "id :: a -> a";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    
    let decl = parser.parse_decl().unwrap();
    
    match decl {
        Decl::TypeSig(name, ty) => {
            assert_eq!(name, "id");
            match ty {
                Type::Function(param, ret) => {
                    assert!(matches!(*param, Type::TypeVar(_)));
                    assert!(matches!(*ret, Type::TypeVar(_)));
                }
                _ => panic!("Expected function type"),
            }
        }
        _ => panic!("Expected TypeSig"),
    }
}

#[test]
fn test_function_matches_signature() {
    let mut type_infer = TypeInference::new();
    
    // register signature: id :: a -> a
    type_infer.register_decl(&Decl::TypeSig(
        "id".to_string(),
        Type::Function(
            Box::new(Type::TypeVar("a".to_string())),
            Box::new(Type::TypeVar("a".to_string()))
        )
    ));
    
    // register definition: id x = x
    type_infer.register_decl(&Decl::FuncDef(
        "id".to_string(),
        vec!["x".to_string()],
        Expr::Identifier("x".to_string())
    ));
    
    // if we've reached here, we never panicked
}

#[test]
fn test_multiple_signatures() {
    let mut type_infer = TypeInference::new();
    
    // add :: Int -> Int -> Int
    type_infer.register_decl(&Decl::TypeSig(
        "add".to_string(),
        Type::Function(
            Box::new(Type::Int),
            Box::new(Type::Function(
                Box::new(Type::Int),
                Box::new(Type::Int)
            ))
        )
    ));
    
    // double :: Int -> Int
    type_infer.register_decl(&Decl::TypeSig(
        "double".to_string(),
        Type::Function(
            Box::new(Type::Int),
            Box::new(Type::Int)
        )
    ));
    
    assert_eq!(type_infer.signatures.len(), 2);
    assert!(type_infer.signatures.contains_key("add"));
    assert!(type_infer.signatures.contains_key("double"));
}