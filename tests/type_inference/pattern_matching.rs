use nexity::ast::ast::{Constraint, Expr, Pattern, Type};
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
fn test_match_expression() {
    let expr = "match 42 { 0 -> False, _ -> True }";
    assert_eq!(infer(expr), Type::Bool);
}

#[test]
fn test_match_wildcard() 
// test for the wildcard
{ 
    let expr = Expr::Match(
        Box::new(Expr::Bool(true)),
        vec![
            (Pattern::Wildcard, None, Expr::Float(10.0))
        ]
    );

    let mut infer = TypeInference::new();
    let ty = infer.infer(&expr).unwrap();
    assert_eq!(ty, (Type::Float, vec![]));
}

#[test]
fn test_match_arm_type_mismatch_fails()
    // Should fail: match x { _ => 1, _ => true }
{ 
    let expr = Expr::Match(
        Box::new(Expr::Int(5)),
        vec![
            (Pattern::Wildcard, None, Expr::Int(1)),
            (Pattern::Wildcard, None, Expr::Bool(true)),
        ]
    );

    let mut infer = TypeInference::new();
    let result = infer.infer(&expr);

    assert!(result.is_err(), "Match branches must unify");
}

#[test]
fn test_match_guard_must_be_bool()
// guard is '5' -> not a boolean
{ 
    let expr = Expr::Match(
        Box::new(Expr::Int(1)),
        vec![
            (
                Pattern::Variable("x".into()),
                Some(Expr::Float(5.0)), // NOT boolean
                Expr::Int(0)
            )
        ]
    );

    let mut infer = TypeInference::new();
    let result = infer.infer(&expr);

    assert!(result.is_err());
}