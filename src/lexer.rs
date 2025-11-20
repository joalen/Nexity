use std::str::Chars;
use std::sync::{Arc, Mutex};

#[derive(Debug, PartialEq, Clone)]
pub enum ReservedToken {
    Case, Classm, Data, Deriving, Do, Else, If, Import, In, Infix, Infixl, Infixr, Instance, Let,
    Of, Module, Newtype, Then, Type, Where, Match, True, False
}

#[derive(Debug, PartialEq, Clone)]
pub enum Token {
    Eof,
    ReserveTok(ReservedToken),
    Identifier(String),
    IntLiteral(i64),
    FloatLiteral(f64),
    Char(char),
    Equals,
    Arrow,
    Pipe,
    DoubleEquals,
    DoubleColon,
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
            
            // comments
            Some('#') => {
                while let Some(ch) = {
                    let current_char = self.current_char.lock().unwrap();
                    *current_char
                } {
                    if ch == '\n' || ch == '\r' { break; }
                    self.next_char();
                }
                self.next_char();
                self.get_token()
            }

            // pipe symbol
            Some('|') => {
                self.next_char();
                Token::Pipe
            }

            // arrow notation
            Some('-') => {
                self.next_char();
                if let Some('>') = {
                    let current_char = self.current_char.lock().unwrap();
                    *current_char
                } {
                    self.next_char();
                    Token::Arrow
                } else {
                    Token::Char('-')
                }
            }

            // the double equals 
            Some('=') => {
                self.next_char();
                if let Some('=') = {
                    let current_char = self.current_char.lock().unwrap();
                    *current_char
                } {
                    self.next_char();
                    Token::DoubleEquals
                } else {
                    Token::Equals
                }
            }

            // comparisons
            Some('<') => {
                self.next_char();
                Token::Char('<')
            }

            Some('>') => {
                self.next_char();
                Token::Char('>')
            }

            // the double colon
            Some(':') => {
                self.next_char();

                if let Some(':') = {
                    let current_char = self.current_char.lock().unwrap();
                    *current_char
                } {
                    self.next_char();
                    Token::DoubleEquals
                } else {
                    Token::Char(':')
                }
            }

            Some(c) => {
                self.next_char();
                Token::Char(c)
            }
            
            None => Token::Eof, // mark end of file
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
            "of"   => Token::ReserveTok(ReservedToken::Of),
            "if"   => Token::ReserveTok(ReservedToken::If),
            "then" => Token::ReserveTok(ReservedToken::Then),
            "else" => Token::ReserveTok(ReservedToken::Else),
            "Classm" => Token::ReserveTok(ReservedToken::Classm),
            "Data" => Token::ReserveTok(ReservedToken::Data),
            "Deriving" => Token::ReserveTok(ReservedToken::Deriving),
            "Do" => Token::ReserveTok(ReservedToken::Do),
            "let" => Token::ReserveTok(ReservedToken::Let),
            "in" => Token::ReserveTok(ReservedToken::In),
            "True" => Token::ReserveTok(ReservedToken::True),
            "False" => Token::ReserveTok(ReservedToken::False),
            _ => Token::Identifier(identifier),
        }
    }

    fn lex_number(&mut self) -> Token {
        let mut num_str = String::new();
        let mut has_decimal = false;

        loop {
            let c = {
                let current_char = self.current_char.lock().unwrap();
                *current_char
            };

            if let Some(c) = c {
                if c == '.' { has_decimal = true; }
                if c.is_digit(10) || c == '.' {
                    num_str.push(c);
                    self.next_char();
                } else { break; }
            } else { break; }
        }

        if has_decimal {
            Token::FloatLiteral(num_str.parse::<f64>().unwrap_or(0.0))
        } else {
            Token::IntLiteral(num_str.parse::<i64>().unwrap_or(0))
        }
    }
}