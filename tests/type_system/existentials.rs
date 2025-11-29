use nexity::{ast::{ast::{Expr, Pattern, Type}, types::TypeInference}, lexer::Lexer, parser::Parser};


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
            Box::new(Type::Function(
                Box::new(Type::TypeVar("a".to_string())),
                Box::new(Type::Int)
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
    
    // Actually assert the structure
    assert_eq!(
        ty,
        Type::Forall(
            vec!["a".to_string()],
            Box::new(Type::Existential(
                vec!["b".to_string()],
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