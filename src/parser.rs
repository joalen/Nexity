use crate::lexer::{Lexer, Token};
use crate::ast::{Expr, BinaryOp};


#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Precedence 
{
    Lowest,
    Pipe, 
    Sum, 
    Product, 
    Application,
    Prefix
}

impl Precedence 
{
    fn from_token(token: &Token) -> Precedence 
    {
        match token 
        {
            Token::Char('+') | Token::Char('-') => Precedence::Sum,
            Token::Char('*') | Token::Char('/') => Precedence::Product,
            Token::Pipe => Precedence::Pipe,
            _ => Precedence::Lowest,
        }
    }
}

pub struct Parser<'a> 
{
    lexer: Lexer<'a>,
    current_token: Token,
}

impl<'a> Parser<'a> 
{
    pub fn new(mut lexer: Lexer<'a>) -> Self 
    {
        let current_token = lexer.get_token();
        Parser { lexer, current_token }
    }

    fn next_token(&mut self)
    {
        let token = self.lexer.get_token();
        self.current_token = token;
    }

    pub fn parse_expr(&mut self, precedence: Precedence) -> Option<Expr> 
    {
        let mut left = self.parse_prefix()?;

        while let Some(op_precedence) = self.current_token_precedence()
        {
            if precedence >= op_precedence 
            {
                break;
            }

            // handling juxtaposition (or basically implicit application)
            if matches!(self.current_token, Token::Identifier(_) | Token::Char('('))
            { 
                left = self.parse_application(left)?;
                continue;
            }

            left = self.parse_infix(left, op_precedence)?;
        }

        Some(left)
    }

    fn parse_function_definition(&mut self) -> Option<Expr> 
    {
        if let Token::Identifier(name) = &self.current_token 
        {
            let function_name = name.clone();
            self.next_token(); // consumator -- function name

            let mut params = Vec::new();
            while let Token::Identifier(param) = &self.current_token 
            {
                params.push(param.clone());
                self.next_token();
            }

            if let Token::Equals = self.current_token 
            {
                self.next_token();

                let body = self.parse_expr(Precedence::Lowest)?;
                return Some(Expr::Function(function_name, Box::new(Expr::Lambda(params, Box::new(body)))));
            }
        }
        None
    }

    fn parse_prefix(&mut self) -> Option<Expr> 
    {
        let token = std::mem::replace(&mut self.current_token, Token::Eof);

        match token 
        {
            Token::Number(n) => 
            {
                self.next_token();
                Some(Expr::Number(n))
            }
            Token::Identifier(id) => 
            {
                let id = id.clone();
                self.next_token();
                Some(Expr::Identifier(id))
            }
            Token::Char('(') => 
            {
                self.next_token();
                let expr = self.parse_expr(Precedence::Lowest);
                if let Token::Char(')') = self.current_token {
                    self.next_token();
                }
                expr
            }
            _ => None,
        }
    }

    fn parse_infix(&mut self, left: Expr, precedence: Precedence) -> Option<Expr> 
    {
        let binary_op = match self.current_token {
            Token::Char('+') => BinaryOp::Add,
            Token::Char('-') => BinaryOp::Subtract,
            Token::Char('*') => BinaryOp::Multiply,
            Token::Char('/') => BinaryOp::Divide,
            Token::Char('%') => BinaryOp::Modulo,

            _ => return None,
        };

        self.next_token(); // consumerator

        let right = self.parse_expr(precedence)?;
        Some(Expr::BinaryOp(Box::new(left), binary_op, Box::new(right)))
    }

    fn parse_lambda(&mut self) -> Option<Expr> 
    {
        self.next_token();
    
        let mut params = Vec::new();
        while let Token::Identifier(param) = &self.current_token 
        {
            params.push(param.clone());
            self.next_token();
        }
    
        if let Token::Arrow = self.current_token 
        {
            self.next_token();
    
            let body = self.parse_expr(Precedence::Lowest)?;
            return Some(Expr::Lambda(params, Box::new(body)));
        }
        None
    }

    fn parse_application(&mut self, func: Expr) -> Option<Expr> 
    {
        let arg = self.parse_expr(Precedence::Lowest)?;
        Some(Expr::Application(Box::new(func), Box::new(arg)))
    }

    fn parse_pipe(&mut self) -> Option<Expr> {
        if let Token::Pipe = self.current_token {
            self.next_token();
            let func = self.parse_expr(Precedence::Lowest)?;
            if let Token::Pipe = self.current_token {
                self.next_token();
                let arg = self.parse_expr(Precedence::Lowest)?;
                return Some(Expr::Pipe(Box::new(func), Box::new(arg)));
            }
        }
        None
    }

    fn current_token_precedence(&self) -> Option<Precedence> 
    {
        Some(Precedence::from_token(&self.current_token))
    }
}
