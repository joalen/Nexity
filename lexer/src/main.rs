mod lexer;
use lexer::{Lexer, Token};
use std::io::{self, Read};



fn main() {
    let mut input = String::new();
    io::stdin().read_to_string(&mut input).expect("Failed to read input");

    let mut lexer = Lexer::new(&input);

    loop {
        let token = lexer.get_token();
        println!("{:?}", token);
        if token == Token::Eof {
            break;
        }
    }
}
