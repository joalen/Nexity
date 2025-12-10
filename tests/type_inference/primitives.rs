use nexity::ast::ast::{BinaryOp, Constraint, Expr, Type};
use nexity::ast::types::{TypeInference};
use nexity::lexer::Lexer;
use nexity::parser::{Parser, Precedence};

fn infer_type_of(input: &str) -> Result<(Type, Vec<Constraint>), String> {
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let expr = parser.parse_expr(Precedence::Lowest)
        .ok_or("Parse failed")?;
    let mut infer = TypeInference::new();
    infer.infer(&expr)
}

fn infer(input: &str) -> Type {
    let (ty, _) = infer_type_of(input).expect("Type inference failed");
    ty
}

#[test]
fn test_int_float_literals() {
    assert_eq!(infer("42"), Type::Int);
    assert_eq!(infer("42.0"), Type::Float);
}

#[test]
fn test_valid_binary_operations_inference()
{ 
    let mut type_infer = TypeInference::new();

    let cases = vec![ 
        (Expr::BinaryOp(
            Box::new(Expr::Int(5)),
            nexity::ast::ast::BinaryOp::Add,
            Box::new(Expr::Int(3))
        ), Type::Int),
    
        (Expr::BinaryOp(
            Box::new(Expr::Int(1)),
            nexity::ast::ast::BinaryOp::Subtract,
            Box::new(Expr::Int(4))
        ), Type::Int),
    
        (Expr::BinaryOp(
            Box::new(Expr::Float(24.5)),
            nexity::ast::ast::BinaryOp::Multiply,
            Box::new(Expr::Float(2.1111))
        ), Type::Float),
        (Expr::BinaryOp(
            Box::new(Expr::Float(20.0)),
            nexity::ast::ast::BinaryOp::Divide,
            Box::new(Expr::Float(5.0))
        ), Type::Float),
    ];

    for (expr, expected_type) in cases 
    { 
        let (inferred_type, _) = type_infer.infer(&expr).unwrap();
        assert_eq!(inferred_type, expected_type);
    }
}

#[test]
fn test_valid_comparison_operations() 
{
    let mut type_infer = TypeInference::new();

    let expr = Expr::BinaryOp(
        Box::new(Expr::Int(42)),
        BinaryOp::Equal,
        Box::new(Expr::Int(25)),
    );

    let inferred_type = type_infer.infer(&expr).unwrap();

    assert_eq!(
        inferred_type,
        (
            Type::Bool,
            vec![
                Constraint {
                    class: "Eq".to_string(),
                    ty: Box::new(Type::Int)
                }
            ]
        )
    );
}
