mod lexer;
mod ast; 
mod parser;
mod llvmgen;

use lexer::{Lexer};
use parser::{Parser, Precedence};

fn main() {
    let input = "10 + 20 * 3 % 5";
    let lexer = Lexer::new(input);
    let mut parser = Parser::new(lexer);
    
    match parser.parse_expr(Precedence::Lowest)
    { 
        Some(ast) => { 
            println!("Parsed AST: {:?}", ast);
        }
        None => { 
            eprintln!("Failure!!")
        }
    }

}