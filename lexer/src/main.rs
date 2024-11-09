mod lexer;
use lexer::{Lexer, Token};
use std::io::{self, Read};



fn main() {
    let input = "let x = 10 + 20 * 3"; // A sample input
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    let ast = Stmt::Let("x".to_string(), parser.parse_expr().unwrap());

    let mut interpreter = Interpreter::new();
    let result = interpreter.interpret(&ast);
    println!("Result: {:?}", result);  // Should output 70
}