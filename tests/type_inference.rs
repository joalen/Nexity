use nexity::ast::ast::{Expr, Type, BinaryOp};
use nexity::ast::types::TypeInference;

#[test]
fn test_valid_binary_operations_inference()
{ 
    let mut type_infer = TypeInference::new();

    let cases = vec![ 
        (Expr::BinaryOp(
            Box::new(Expr::Number(5.0)),
            nexity::ast::ast::BinaryOp::Add,
            Box::new(Expr::Number(3.0))
        ), Type::Float),
    
        (Expr::BinaryOp(
            Box::new(Expr::Number(10.0)),
            nexity::ast::ast::BinaryOp::Subtract,
            Box::new(Expr::Number(4.0))
        ), Type::Float),
    
        (Expr::BinaryOp(
            Box::new(Expr::Number(24.0)),
            nexity::ast::ast::BinaryOp::Multiply,
            Box::new(Expr::Number(2.0))
        ), Type::Float),
        (Expr::BinaryOp(
            Box::new(Expr::Number(20.0)),
            nexity::ast::ast::BinaryOp::Divide,
            Box::new(Expr::Number(5.0))
        ), Type::Float),
    ];

    for (expr, expected_type) in cases 
    { 
        let inferred_type = type_infer.infer(&expr).unwrap();
        assert_eq!(inferred_type, expected_type);
    }
}

#[test]
fn test_invalid_binary_operations_inference() 
{
    let mut type_infer = TypeInference::new();

    // can't add aa number and a boolean togethers
    let expr = Expr::BinaryOp(
        Box::new(Expr::Number(42.0)),
        BinaryOp::Add,
        Box::new(Expr::Bool(true)),
    );

    match type_infer.infer(&expr) {
        Ok(_) => panic!("Expected type error!"),
        Err(err) => assert_eq!(err.contains( "Type error in binary operation."), true),
    }

    // invalid types for doing a multiplication of a float + string
    let expr = Expr::BinaryOp(
        Box::new(Expr::Number(42.0)),
        BinaryOp::Multiply,
        Box::new(Expr::String("a string".to_string())),
    );


    match type_infer.infer(&expr) {
        Ok(_) => panic!("Expected type error!"),
        Err(err) => {
            assert_eq!(err.contains("Type error in binary operation."), true);
        }
    }
}

#[test]
fn test_valid_comparison_operations() 
{
    let mut type_infer = TypeInference::new();

    let expr = Expr::BinaryOp(
        Box::new(Expr::Number(42.0)),
        BinaryOp::Equal,
        Box::new(Expr::Number(25.0)), 
    );

    let inferred_type = type_infer.infer(&expr).unwrap();
    assert_eq!(inferred_type, Type::Bool);
}