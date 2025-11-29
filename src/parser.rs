use crate::lexer::{Lexer, ReservedToken, Token};
use crate::ast::ast::{BinaryOp, Constructor, Decl, Expr, MethodImpl, MethodSig, Pattern, Type};


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
    pub current_token: Token,
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

        // look for explicit type annotations
        if self.current_token == Token::DoubleColon {
            self.next_token();
            let ty = self.parse_type()?;
            left = Expr::Annotated(Box::new(left), ty);
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
                self.next_token();
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

    fn parse_infix(&mut self, left: Expr, _precedence: Precedence) -> Option<Expr> 
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
        let arg = self.parse_prefix()?;
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
    
            let mut rhs = self.parse_expr(Precedence::Application)?;
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

    pub fn parse_type(&mut self) -> Option<Type> {
        // check for existential operator
        if self.current_token == Token::ReserveTok(ReservedToken::Exists) {
            self.next_token(); // consume 'exists'
            
            let mut vars = Vec::new();
            while let Token::Identifier(v) = &self.current_token {
                if v.chars().next().unwrap().is_lowercase() {
                    vars.push(v.clone());
                    self.next_token();
                } else {
                    break;
                }
            }
            
            if self.current_token != Token::Char('.') {
                return None;
            }
            self.next_token(); // consume '.'
            
            let inner_type = self.parse_type()?;
            return Some(Type::Existential(vars, Box::new(inner_type)));
        }

        // check for the forall operator 
        if self.current_token == Token::ReserveTok(ReservedToken::Forall)
        { 
            self.next_token(); // consume 'forall' 

            let mut vars = Vec::new();
            while let Token::Identifier(v) = &self.current_token
            { 
                if v.chars().next().unwrap().is_lowercase()
                {
                    vars.push(v.clone());
                    self.next_token();
                } else { 
                    break;
                }
            }

            if self.current_token != Token::Char('.') 
            { 
                return None;
            }

            self.next_token(); // consume the '.'

            let inner_type = self.parse_type()?;
            return Some(Type::Forall(vars, Box::new(inner_type)));
        }

        // Parse base type
        let mut base_ty = match &self.current_token {
            Token::Identifier(name) => {
                let ty = match name.as_str() {
                    "Int" => Type::Int,
                    "Float" => Type::Float,
                    "Bool" => Type::Bool,
                    "Char" => Type::Char,
                    _ => 
                    { 
                        if name.chars().next().unwrap().is_lowercase() {
                            Type::TypeVar(name.clone())
                        } else {
                            Type::Custom(name.clone())
                        }
                    }
                };
                self.next_token();
                ty
            }

            Token::Char('(') => {
                self.next_token();
                let inner = self.parse_type()?;
                if self.current_token == Token::Char(')') {
                    self.next_token();
                }
                inner
            }
            _ => return None,
        };

        // collect type argumentss (for type application)
        let mut args = Vec::new();
        while let Some(arg) = self.try_parse_type_arg() {
            args.push(arg);
        }

        // If we have arguments, wrap in a Type::Apply
        if !args.is_empty() {
            base_ty = Type::Apply(Box::new(base_ty), args);
        }

        if self.current_token == Token::Arrow {
            self.next_token();
            let ret = self.parse_type()?;
            return Some(Type::Function(Box::new(base_ty), Box::new(ret)));
        }

        Some(base_ty)
    }

    fn try_parse_type_arg(&mut self) -> Option<Type> {
        match &self.current_token {
            Token::Identifier(name) if name.chars().next().unwrap().is_lowercase() => {
                // Could be a type variable
                let ty = Type::TypeVar(name.clone());
                self.next_token();
                Some(ty)
            }
            Token::Identifier(name) if name.chars().next().unwrap().is_uppercase() => {
                // Could be a type constructor
                let ty = Type::Custom(name.clone());
                self.next_token();
                Some(ty)
            }
            Token::Char('(') => {
                // Parenthesized type
                self.next_token();
                let inner = self.parse_type()?;
                if self.current_token == Token::Char(')') {
                    self.next_token();
                }
                Some(inner)
            }
            _ => None
        }
    }

    fn parse_class(&mut self) -> Option<Decl> {
        self.next_token(); // consume 'class'
        
        // Get class name
        let class_name = if let Token::Identifier(name) = &self.current_token {
            name.clone()
        } else {
            return None;
        };
        self.next_token();
        
        // Get type variable
        let type_var = if let Token::Identifier(var) = &self.current_token {
            var.clone()
        } else {
            return None;
        };
        self.next_token();
        
        // Expect 'where'
        if self.current_token != Token::ReserveTok(ReservedToken::Where) {
            return None;
        }
        self.next_token();
        
        // Expect '{'
        if self.current_token != Token::Char('{') {
            return None;
        }
        self.next_token();
        
        // Parse method signatures
        let mut methods = Vec::new();
        while self.current_token != Token::Char('}') {
            if let Token::Identifier(method_name) = &self.current_token {
                let name = method_name.clone();
                self.next_token();
                
                // Expect '::'
                if self.current_token != Token::DoubleColon {
                    return None;
                }
                self.next_token();
                
                // Parse type
                let ty = self.parse_type()?;
                methods.push(MethodSig { name, ty });
                
                // Optional semicolon
                if self.current_token == Token::Char(';') {
                    self.next_token();
                }
            } else {
                break;
            }
        }
        
        // Expect '}'
        if self.current_token == Token::Char('}') {
            self.next_token();
        }
        
        Some(Decl::Class(class_name, type_var, methods))
    }
    
    fn parse_instance(&mut self) -> Option<Decl> {
        self.next_token(); // consume 'instance'
        
        // Get class name
        let class_name = if let Token::Identifier(name) = &self.current_token {
            name.clone()
        } else {
            return None;
        };
        self.next_token();
        
        // Parse the type
        let ty = self.parse_type()?;
        
        // Expect 'where'
        if self.current_token != Token::ReserveTok(ReservedToken::Where) {
            return None;
        }
        self.next_token();
        
        // Expect '{'
        if self.current_token != Token::Char('{') {
            return None;
        }
        self.next_token();
        
        // Parse method implementations
        let mut methods = Vec::new();
        while self.current_token != Token::Char('}') {
            if let Token::Identifier(method_name) = &self.current_token {
                let name = method_name.clone();
                self.next_token();
                
                // Expect '='
                if self.current_token != Token::Equals {
                    return None;
                }
                self.next_token();
                
                // Parse body expression
                let body = self.parse_expr(Precedence::Lowest)?;
                methods.push(MethodImpl { name, body });
                
                // Optional semicolon
                if self.current_token == Token::Char(';') {
                    self.next_token();
                }
            } else {
                break;
            }
        }
        
        // Expect '}'
        if self.current_token == Token::Char('}') {
            self.next_token();
        }
        
        Some(Decl::Instance(class_name, ty, methods))
    }

    fn parse_data(&mut self) -> Option<Decl> 
    { 
        self.next_token(); // consuming the 'data' token 

        let name = match &self.current_token
        { 
            Token::Identifier(n) => n.clone(),
            _ => return None,
        };

        self.next_token(); // consume name

        // Type params 
        let mut type_params = Vec::new();
        while let Token::Identifier(param) = &self.current_token {
            if param.chars().next().unwrap().is_lowercase() {
                type_params.push(param.clone());
                self.next_token();
            } else {
                break;
            }
        }

        // check for GADTs
        if self.current_token == Token::ReserveTok(ReservedToken::Where)
        { 
            return self.parse_gadt(name, type_params);
        }

        // Expect the equal sign
        if self.current_token != Token::Equals 
        { 
            return None;
        }

        // parse constructors 
        let mut constructors = Vec::new(); 
        loop 
        { 
            let ctor_name = match &self.current_token
            { 
                Token::Identifier(n) => n.clone(), 
                _ => break,
            };

            self.next_token(); // finally, consume constructor name

            let mut fields = Vec::new();
            if self.current_token == Token::Char('{') {
                self.next_token();
                while self.current_token != Token::Char('}') {
                    let field_name = match &self.current_token {
                        Token::Identifier(n) => n.clone(),
                        _ => break,
                    };
                    self.next_token();
                    
                    if self.current_token != Token::DoubleColon { return None; }
                    self.next_token();
                    
                    let ty = self.parse_type()?;
                    fields.push((Some(field_name), ty));
                    
                    if self.current_token == Token::Char(',') {
                        self.next_token();
                    }
                }
                self.next_token(); // consume '}'
            } else {
                // Positional fields (existing code)
                while let Some(ty) = self.parse_type() {
                    fields.push((None, ty));
                }
            }

            // get field types
            let mut fields = Vec::new(); 
            while let Some(ty) = self.parse_type()
            { 
                fields.push(ty);
            }

            constructors.push(Constructor { name: ctor_name, fields, result_ty: None });

            // check for the '|' to see if we continue or break 
            if self.current_token == Token::Char('|') 
            { 
                self.next_token(); // consume '|'
            } else { 
                break;
            }
        }

        Some(Decl::Data(name, type_params, constructors))

    }

    fn parse_gadt(&mut self, name: String, type_params: Vec<String>) -> Option<Decl> 
    { 
        self.next_token(); // consume the 'where' 

        if self.current_token != Token::Char('{') 
        { 
            return None;
        }

        self.next_token(); // consume '{'

        let mut constructors = Vec::new();

        while self.current_token != Token::Char('}')
        { 
            let ctor_name = match &self.current_token {
                Token::Identifier(n) => n.clone(),
                _ => break,
            };

            self.next_token(); // consume constructor name 

            if self.current_token != Token::DoubleColon
            { 
                return None;
            }

            self.next_token();

            let full_type = self.parse_type()?;

            let (fields, result_ty) = self.decompose_function_type(full_type);

            constructors.push(Constructor {
                name: ctor_name,
                fields,
                result_ty: Some(result_ty),
            });

            if self.current_token == Token::Char(';') {
                self.next_token();
            }
        }

        self.next_token();
        Some(Decl::Data(name, type_params, constructors))
    }

    fn decompose_function_type(&self, ty: Type) -> (Vec<Type>, Type)
    { 
        let mut fields = Vec::new();
        let mut current = ty;
        
        while let Type::Function(param, ret) = current {
            fields.push(*param);
            current = *ret;
        }
        
        (fields, current)
    }

    fn parse_type_alias(&mut self) -> Option<Decl>
    { 
        self.next_token(); // consume 'type'

        let name = match &self.current_token
        { 
            Token::Identifier(n) => n.clone(), 
            _ => return None,
        }; 

        self.next_token();

        let mut type_params = Vec::new();
        while let Token::Identifier(param) = &self.current_token
        { 
            if param.chars().next().unwrap().is_lowercase()
            { 
                type_params.push(param.clone());
                self.next_token();
            } else { 
                break;
            }
        }

        if self.current_token != Token::Equals
        { 
            return None;
        }

        self.next_token();

        let ty = self.parse_type()?;
        Some(Decl::TypeAlias(name, type_params, ty))
    }

    fn current_token_precedence(&self) -> Option<Precedence> 
    {
        Some(Precedence::from_token(&self.current_token))
    }
}
