#![allow(dead_code)]

use std::str::Chars;

#[derive(Debug, PartialEq, Clone)]
pub enum ReservedToken {
    Case, Classm, Data, Deriving, Do, Else, If, Import, In, Infix, Infixl, Infixr, Instance, Let,
    Of, Module, Newtype, Then, Type, Where, Match, True, False, Class, Forall, Exists,
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
    DoubleArrow,
    VirtualSemi,
}

pub struct Lexer<'a> {
    chars: Chars<'a>,
    current: Option<char>,
    col: usize,
    layout_col: Option<usize>,
    pending_virtual_semi: bool,
    pub indent_stack: Vec<usize>,
    push_next_col: bool,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Lexer {
            chars: input.chars(),
            current: None,
            col: 0,
            layout_col: None,
            pending_virtual_semi: false,
            indent_stack: Vec::new(),
            push_next_col: false,
        };
        lexer.advance();
        lexer
    }

    fn advance(&mut self) {
        self.current = self.chars.next();
        match self.current {
            Some('\n') => { self.col = 0; }
            Some(_)    => { self.col += 1; }
            None       => {}
        }
    }

    fn peek(&self) -> Option<char> {
        self.current
    }

    // returns true if crossed a newline back to col 1 (new top-level decl)
    fn skip_whitespace(&mut self) -> bool {
        let mut crossed_newline = false;
        loop {
            match self.current {
                Some(c) if c.is_whitespace() => {
                    if c == '\n' { crossed_newline = true; }
                    self.advance();
                }
                _ => break,
            }
        }
        if !crossed_newline { return false; }

        while self.indent_stack.len() > 1 && self.col < *self.indent_stack.last().unwrap() {
            self.indent_stack.pop();
        }

        self.indent_stack.last().map_or(false, |&layout| self.col == layout)
    }

    pub fn get_token(&mut self) -> Token {
        if self.pending_virtual_semi {
            self.pending_virtual_semi = false;
            return Token::VirtualSemi;
        }

        let emit_semi = self.skip_whitespace();

        if self.push_next_col && self.current.is_some() {
            self.indent_stack.push(self.col);
            self.push_next_col = false;
        } else { 
            if self.layout_col.is_none() {
                if self.current.is_some() {
                    self.layout_col = Some(self.col);
                    self.indent_stack.push(self.col);
                }
            }

            if emit_semi {
                return Token::VirtualSemi;
            }
        }

        let tok = match self.current {
            Some(c) if c.is_alphabetic() => self.lex_identifier(),
            Some(c) if c.is_ascii_digit() => self.lex_number(),

            Some('.') => { self.advance(); Token::Char('.') }

            Some('#') => {
                while matches!(self.current, Some(c) if c != '\n' && c != '\r') {
                    self.advance();
                }
                self.advance();
                self.get_token()
            }

            Some('|') => { self.advance(); Token::Pipe }

            Some('-') => {
                self.advance();
                if self.current == Some('>') { self.advance(); Token::Arrow }
                else { Token::Char('-') }
            }

            Some('=') => {
                self.advance();
                match self.current {
                    Some('=') => { self.advance(); Token::DoubleEquals }
                    Some('>') => { self.advance(); Token::DoubleArrow }
                    _         => Token::Equals,
                }
            }

            Some('<') => { self.advance(); Token::Char('<') }
            Some('>') => { self.advance(); Token::Char('>') }

            Some(':') => {
                self.advance();
                if self.current == Some(':') { self.advance(); Token::DoubleColon }
                else { Token::Char(':') }
            }

            Some(c) => { self.advance(); Token::Char(c) }
            None    => Token::Eof,
        };

        if tok == Token::ReserveTok(ReservedToken::Where) {
            self.skip_whitespace();
            if self.current.is_some() {
                self.indent_stack.push(self.col);
            }
        }

        tok
    }

    pub fn pop_layout(&mut self) {
        if self.indent_stack.len() > 1 {
            self.indent_stack.pop();
        }
    }

    fn lex_identifier(&mut self) -> Token {
        let mut s = String::new();
        while let Some(c) = self.current {
            if c.is_alphanumeric() || c == '_' { s.push(c); self.advance(); }
            else { break; }
        }
        match s.as_str() {
            "case"     => Token::ReserveTok(ReservedToken::Case),
            "of"       => Token::ReserveTok(ReservedToken::Of),
            "if"       => Token::ReserveTok(ReservedToken::If),
            "then"     => Token::ReserveTok(ReservedToken::Then),
            "else"     => Token::ReserveTok(ReservedToken::Else),
            "Classm"   => Token::ReserveTok(ReservedToken::Classm),
            "Data"     => Token::ReserveTok(ReservedToken::Data),
            "Deriving" => Token::ReserveTok(ReservedToken::Deriving),
            "Do"       => Token::ReserveTok(ReservedToken::Do),
            "let"      => Token::ReserveTok(ReservedToken::Let),
            "in"       => Token::ReserveTok(ReservedToken::In),
            "True"     => Token::ReserveTok(ReservedToken::True),
            "False"    => Token::ReserveTok(ReservedToken::False),
            "type"     => Token::ReserveTok(ReservedToken::Type),
            "forall"   => Token::ReserveTok(ReservedToken::Forall),
            "exists"   => Token::ReserveTok(ReservedToken::Exists),
            "where"    => Token::ReserveTok(ReservedToken::Where),
            _          => Token::Identifier(s),
        }
    }

    fn lex_number(&mut self) -> Token {
        let mut s = String::new();
        let mut has_dot = false;
        while let Some(c) = self.current {
            if c.is_ascii_digit() || c == '.' {
                if c == '.' { has_dot = true; }
                s.push(c); self.advance();
            } else { break; }
        }
        if has_dot { Token::FloatLiteral(s.parse().unwrap_or(0.0)) }
        else       { Token::IntLiteral(s.parse().unwrap_or(0)) }
    }
}