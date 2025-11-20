use crate::lexer::{Lexer, ReservedToken, Token};
use crate::ast::ast::{Expr, BinaryOp, Pattern};


#[derive(Debug, PartialEq, PartialOrd, Clone, Copy)]
pub enum Precedence 
{
    Lowest,
    Pipe, 
    Comparison,
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
            Token::DoubleEquals => Precedence::Comparison,
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
            self.next_token();

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
            Token::IntLiteral(n) => {
                self.next_token();
                Some(Expr::Int(n))
            }
            
            Token::FloatLiteral(n) => {
                self.next_token();
                Some(Expr::Float(n))
            }

            Token::Identifier(id) => 
            {
                let id = id.clone();
    
                if id == "match" 
                { 
                    self.next_token();
                    return self.parse_match();
                }
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

            Token::Char('\\') => {
                self.parse_lambda()
            }
            
            // bear with me for this section...I do want to organize this, but at the moment, Rust has a weird way of handling submodules and it'll just be a rat's nest of issues I would have to unravel
            Token::ReserveTok(ReservedToken::If) => {
                self.next_token();
                let cond = self.parse_expr(Precedence::Lowest)?;
                
                if self.current_token != Token::ReserveTok(ReservedToken::Then) {
                    return None;
                }
                self.next_token();
                let then_expr = self.parse_expr(Precedence::Lowest)?;
                
                if self.current_token != Token::ReserveTok(ReservedToken::Else) {
                    return None;
                }
                self.next_token();
                let else_expr = self.parse_expr(Precedence::Lowest)?;
                
                Some(Expr::If(Box::new(cond), Box::new(then_expr), Box::new(else_expr)))
            }

            Token::ReserveTok(ReservedToken::Let) => {
                return self.parse_let_expression();
            }

            Token::ReserveTok(ReservedToken::True) => {
                self.next_token();
                Some(Expr::Bool(true))
            },
            Token::ReserveTok(ReservedToken::False) => {
                self.next_token();
                Some(Expr::Bool(false))
            },

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
            Token::DoubleEquals => BinaryOp::Equal,
            Token::Char('<') => BinaryOp::LessThan,
            Token::Char('>') => BinaryOp::GreaterThan,

            _ => return None,
        };

        let token_precedence = Precedence::from_token(&self.current_token);
        self.next_token(); // consumerator


        let right = self.parse_expr(token_precedence)?;
        Some(Expr::BinaryOp(Box::new(left), binary_op, Box::new(right)))
    }

    fn parse_lambda(&mut self) -> Option<Expr> 
    {    
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

    fn parse_pattern(&mut self) -> Option<Pattern> 
    { 
        match &self.current_token 
        {
            Token::IntLiteral(n) => {
                let n = *n;
                self.next_token();
                Some(Pattern::Literal(n as f64))
            }
            
            Token::FloatLiteral(n) => {
                let n = *n;
                self.next_token();
                Some(Pattern::Literal(n))
            }

            Token::Identifier(name) => 
            { 
                let n = name.clone();
                self.next_token();
                Some(Pattern::Variable(n))
            }

            Token::Char('_') => 
            { 
                self.next_token();
                Some(Pattern::Wildcard)
            }

            _ => None,
        }
    }

    fn parse_match(&mut self) -> Option<Expr> 
    { 
        // consumed already by parse_prefix() 
        let scrutinee = self.parse_expr(Precedence::Lowest)?;

        if self.current_token != Token::Char('{') 
        {
            return None;
        }

        self.next_token(); // consume 

        let mut arms = Vec::new();

        while self.current_token != Token::Char('}')
        { 
            let pat = self.parse_pattern()?;

            // optional guarding for <expr> 
            let guard = if let Token::Identifier(id) = &self.current_token {
                if id == "if" {
                    self.next_token(); // consume 'if'
                    Some(self.parse_expr(Precedence::Lowest)?)
                } else {
                    None
                }
            } else {
                None
            };

            // expect the => 
            if self.current_token != Token::Arrow {
                return None;
            }
            self.next_token(); // consume '=>'

            // parse the resulting expression 
            let body = self.parse_expr(Precedence::Lowest)?;
            arms.push((pat, guard, body));

            // consume any optional comma after getting resultant expression
            if self.current_token == Token::Char(',') {
                self.next_token();
            }
        }

        self.next_token(); // consume the closing '}'
        Some(Expr::Match(Box::new(scrutinee), arms)) // fully parsed out match
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

    fn parse_let_expression(&mut self) -> Option<Expr> {
        self.next_token(); // consume 'let'
    
        let mut bindings: Vec<(String, Expr)> = Vec::new();
    
        loop {
            if self.current_token == Token::ReserveTok(ReservedToken::In) 
                || self.current_token == Token::Eof {
                break;
            }
            
            let name = if let Token::Identifier(n) = &self.current_token {
                n.clone()
            } else {
                break;
            };

            self.next_token();
    
            let mut params = Vec::new();
            while let Token::Identifier(param) = &self.current_token {
                params.push(param.clone());
                self.next_token();
            }
    
            if self.current_token != Token::Equals {
                return None;
            }
            self.next_token(); // consume '='
    
            let mut rhs = self.parse_expr(Precedence::Lowest)?;
            if !params.is_empty() {
                rhs = Expr::Lambda(params, Box::new(rhs));
            }
            
            bindings.push((name, rhs));
    
            if self.current_token == Token::Char(';') {
                self.next_token();
                continue; // optional
            }

            if self.current_token == Token::ReserveTok(ReservedToken::In) 
                || self.current_token == Token::Eof {
                break;
            }

            if !matches!(self.current_token, Token::Identifier(_)) {
                break;
            }
        }
    
        // Parse 'in' body
        if self.current_token == Token::ReserveTok(ReservedToken::In) {
            self.next_token();
            let body = self.parse_expr(Precedence::Lowest)?;
            Some(Expr::Let(bindings, Box::new(body)))
        } else {
            // No 'in', treat last binding as body
            let last = bindings.pop()?;
            Some(Expr::Let(bindings, Box::new(last.1)))
        }
    }

    fn current_token_precedence(&self) -> Option<Precedence> 
    {
        Some(Precedence::from_token(&self.current_token))
    }
}
