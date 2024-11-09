use std::str::Chars;
use std::sync::{Arc, Mutex};

#[derive(Debug, PartialEq)]
pub enum ReservedToken {
    Case, Classm, Data, Deriving, Do, Else, If, Import, In, Infix, Infixl, Infixr, Instance, Let,
    Of, Module, Newtype, Then, Type, Where,
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Eof,
    ReserveTok(ReservedToken),
    Identifier(String),
    Number(f64),
    Char(char),
    Equals,
    Arrow,
    Pipe,
}

pub struct Lexer<'a> {
    input: Arc<Mutex<Chars<'a>>>,
    current_char: Arc<Mutex<Option<char>>>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input: Arc::new(Mutex::new(input.chars())),
            current_char: Arc::new(Mutex::new(None)),
        };
        lexer.next_char();
        lexer
    }

    fn next_char(&mut self) {
        let mut input = self.input.lock().unwrap();
        let mut current_char = self.current_char.lock().unwrap();
        *current_char = input.next();
    }

    fn skip_whitespace(&mut self) {
        loop {
            let c = {
                let current_char = self.current_char.lock().unwrap();
                *current_char
            };

            if let Some(c) = c {
                if c.is_whitespace() {
                    self.next_char();
                } else {
                    break;
                }
            } else {
                break;
            }
        }
    }

    pub fn get_token(&mut self) -> Token {
        self.skip_whitespace();

        let current_char = {
            let current_char = self.current_char.lock().unwrap();
            *current_char
        };

        match current_char {
            Some(c) if c.is_alphabetic() => self.lex_identifier(),
            Some(c) if c.is_digit(10) || c == '.' => self.lex_number(),
            Some('#') => {
                loop {
                    let c = {
                        let current_char = self.current_char.lock().unwrap();
                        *current_char
                    };

                    if let Some(c) = c {
                        if c == '\n' || c == '\r' {
                            break;
                        }
                        self.next_char();
                    } else {
                        break;
                    }
                }
                self.next_char();
                self.get_token()
            }
            Some(c) => {
                self.next_char();
                Token::Char(c)
            }
            None => Token::Eof,
        }
    }

    fn lex_identifier(&mut self) -> Token {
        let mut identifier = String::new();

        if let Some(c) = {
            let current_char = self.current_char.lock().unwrap();
            *current_char
        } {
            identifier.push(c);
            self.next_char();
        }

        loop {
            let c = {
                let current_char = self.current_char.lock().unwrap();
                *current_char
            };

            if let Some(c) = c {
                if c.is_alphanumeric() {
                    identifier.push(c);
                    self.next_char();
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        match identifier.as_str() {
            "case" => Token::ReserveTok(ReservedToken::Case), 
            "Classm" => Token::ReserveTok(ReservedToken::Classm),
            "Data" => Token::ReserveTok(ReservedToken::Data),
            "Deriving" => Token::ReserveTok(ReservedToken::Deriving),
            "Do" => Token::ReserveTok(ReservedToken::Do),
            _ => Token::Identifier(identifier),
        }
    }

    fn lex_number(&mut self) -> Token {
        let mut num_str = String::new();

        loop {
            let c = {
                let current_char = self.current_char.lock().unwrap();
                *current_char
            };

            if let Some(c) = c {
                if c.is_digit(10) || c == '.' {
                    num_str.push(c);
                    self.next_char();
                } else {
                    break;
                }
            } else {
                break;
            }
        }

        match num_str.parse::<f64>() {
            Ok(num_val) => Token::Number(num_val),
            Err(_) => Token::Number(0.0),
        }
    }
}