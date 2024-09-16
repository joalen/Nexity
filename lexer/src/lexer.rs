use std::str::Chars;


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
}

pub struct Lexer<'a> {
    input: Chars<'a>,
    current_char: Option<char>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            input: input.chars(),
            current_char: None,
        };
        lexer.next_char();
        lexer
    }

    fn next_char(&mut self) {
        self.current_char = self.input.next();
    }

    fn skip_whitespace(&mut self) {
        while let Some(c) = self.current_char {
            if c.is_whitespace() {
                self.next_char();
            } else {
                break;
            }
        }
    }

    pub fn get_token(&mut self) -> Token {
        self.skip_whitespace();

        match self.current_char {
            Some(c) if c.is_alphabetic() => self.lex_identifier(),
            Some(c) if c.is_digit(10) || c == '.' => self.lex_number(),
            Some('#') => {
                // Skip comments until the end of the line
                while let Some(c) = self.current_char {
                    if c == '\n' || c == '\r' {
                        break;
                    }
                    self.next_char();
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
        if let Some(c) = self.current_char {
            identifier.push(c);
            self.next_char();
        }

        while let Some(c) = self.current_char {
            if c.is_alphanumeric() {
                identifier.push(c);
                self.next_char();
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
        while let Some(c) = self.current_char {
            if c.is_digit(10) || c == '.' {
                num_str.push(c);
                self.next_char();
            } else {
                break;
            }
        }

        let num_val = num_str.parse::<f64>().unwrap_or(0.0);
        Token::Number(num_val)
    }
}