use nexity::ast::ast::{Constraint, Type};
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

#[test]
fn test_reject_polymorphic_recursion() {
    let code = r#"
        let f = \x -> (f [x], f x)
    "#;

    assert!(infer_type_of(code).is_err());
}