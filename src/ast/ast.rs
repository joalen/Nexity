use std::collections::HashMap;

pub enum Decl {
    Data(String, Vec<String>, Vec<Constructor>),
    TypeAlias(String, Vec<String>, Type),
    Class(String, String, Vec<MethodSig>),
    Instance(String, Type, Vec<MethodImpl>),
}

pub struct MethodSig {
    pub name: String,
    pub ty: Type,
}

pub struct MethodImpl {
    pub name: String,
    pub body: Expr,
}

pub struct Constructor {
    pub name: String,
    pub fields: Vec<Type>,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    IntLiteral(i64),
    FloatLiteral(f64),
    Bool(bool),
    Function(Vec<String>, Box<Expr>, Env), 
}

// Type alias for the environment
pub type Env = HashMap<String, Value>;

#[derive(Debug, PartialEq, Clone)]
pub enum Type {
    Int,
    Float,
    Bool,
    Char,
    Function(Box<Type>, Box<Type>),
    Custom(String),
    TypeVar(String),
    Constrained(Vec<Constraint>, Box<Type>),
    Constructor(String), 
    Apply(Box<Type>, Vec<Type>)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Constraint {
    pub class: String,
    pub ty: Box<Type>,
}

#[derive(Debug, PartialEq, Clone)] 
pub enum Expr {
    Int(i64),
    Float(f64),
    Identifier(String),
    String(String),
    BinaryOp(Box<Expr>, BinaryOp, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Function(String, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Return(Box<Expr>),
    Lambda(Vec<String>, Box<Expr>),
    Application(Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<(Pattern, Option<Expr>, Expr)>),
    Pipe(Box<Expr>, Box<Expr>),
    Let(Vec<(String, Expr)>, Box<Expr>),
    While(Box<Expr>, Box<Expr>),
    Where(Box<Expr>, Vec<(String, Expr)>),
    Bool(bool),
    Not(Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Annotated(Box<Expr>, Type),
}

// Binary operations supported in the language
#[derive(Debug, PartialEq, Clone)] // Derive Clone for simple BinaryOp variants
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
    Modulo,
    And,
    Or,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Literal(f64),
    Variable(String),
    Constructor(String, Vec<Pattern>),
    Wildcard,
}

impl Expr {
    fn apply_function(&self, func: Value, arg: Value) -> Result<Value, String> {
        if let Value::Function(params, body, closure_env) = func {
            if params.len() != 1 {
                return Err("Function arity mismatch".into());
            }
            let mut new_env = closure_env.clone();
            new_env.insert(params[0].clone(), arg);
            body.evaluate(&mut new_env)
        } else {
            Err("Attempted to call a non-function value".into())
        }
    }

    fn matches_pattern(&self, value: &Value, pattern: &Pattern, env: &mut Env) -> bool {
        match (value, pattern) {
            (Value::IntLiteral(n), Pattern::Literal(p)) => (*n as f64) == *p,
            (Value::FloatLiteral(n), Pattern::Literal(p)) => *n == *p,
            (Value::Bool(b), Pattern::Literal(p)) => *b == (*p != 0.0),
            (v, Pattern::Variable(name)) => {
                env.insert(name.clone(), v.clone());
                true
            }
            (_, Pattern::Wildcard) => true,
            _ => false,
        }
    }

    pub fn evaluate(&self, env: &mut Env) -> Result<Value, String> {
        match self {
            Expr::Int(n)   => Ok(Value::IntLiteral(*n)),
            Expr::Float(n) => Ok(Value::FloatLiteral(*n)),
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::Identifier(name) => {
                env.get(name)
                .cloned()
                .ok_or_else(|| format!("Undefined identifier: {}", name))
            }

            Expr::BinaryOp(lhs, op, rhs) => {
                let left = lhs.evaluate(env)?;
                let right = rhs.evaluate(env)?;
                self.eval_binary_op(left, op, right)
            }

            Expr::If(cond, then_branch, else_branch) => {
                let condition = cond.evaluate(env)?;
                if self.is_truthy(&condition) {
                    then_branch.evaluate(env)
                } else {
                    else_branch.evaluate(env)
                }
            }

            Expr::Let(bindings, body) => {
                let mut local_env = env.clone();
                for (name, _) in bindings {
                    local_env.insert(name.clone(), Value::Bool(false)); // dummy placeholder
                }
            
                // 2. Evaluate each binding in the environment containing placeholders
                let mut evaluated_bindings = HashMap::new();
                for (name, expr) in bindings {
                    let value = expr.evaluate(&mut local_env)?;
                    local_env.insert(name.clone(), value.clone());
                    evaluated_bindings.insert(name.clone(), value);
                }
            
                // 3. Evaluate the body using the fully populated local environment
                body.evaluate(&mut local_env)
            }

            Expr::Lambda(params, body) => {
                Ok(Value::Function(params.clone(), body.clone(), env.clone()))
            }

            Expr::Application(func_expr, arg_expr) => {
                let func = func_expr.evaluate(env)?;
                let arg = arg_expr.evaluate(env)?;
                self.apply_function(func, arg)
            }

            Expr::Pipe(func_expr, arg_expr) => {
                let func = func_expr.evaluate(env)?;
                let arg = arg_expr.evaluate(env)?;
            
                if let Value::Function(params, body, closure_env) = func {
                    let mut local_env = closure_env.clone();

                    if params.len() == 1 {
                        local_env.insert(params[0].clone(), arg);
                    } else {
                        return Err("Function expects one parameter".into());
                    }
            
                    let result = body.evaluate(&mut local_env)?;
                    Ok(result)
                } else {
                    Err("Pipe expects a function on the left-hand side".into())
                }
            }

            Expr::Annotated(expr, _) => expr.evaluate(env),

            Expr::Match(expr, cases) => {
                let value = expr.evaluate(env)?;
                for (pattern, guard, result) in cases {
                    if self.matches_pattern(&value, pattern, env) {
                        if let Some(guard_expr) = guard {
                            if !self.is_truthy(&guard_expr.evaluate(env)?) {
                                continue;
                            }
                        }
                        return result.evaluate(env);
                    }
                }
                Err("No matching pattern found".into())
            }

            _ => Err("Unsupported expression".into()),
        }
    }

    fn promote(lhs: Value, rhs: Value) -> (Value, Value) {
        match (lhs, rhs) {
            (Value::IntLiteral(a), Value::IntLiteral(b)) =>
                (Value::FloatLiteral(a as f64), Value::FloatLiteral(b as f64)),
    
            (Value::IntLiteral(a), Value::FloatLiteral(b)) =>
                (Value::FloatLiteral(a as f64), Value::FloatLiteral(b)),
    
            (Value::FloatLiteral(a), Value::IntLiteral(b)) =>
                (Value::FloatLiteral(a), Value::FloatLiteral(b as f64)),
    
            other => other,
        }
    }

    fn eval_binary_op(&self, left: Value, op: &BinaryOp, right: Value) -> Result<Value, String> {
        use Value::*;
    
        match op {
            BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide => {
                // Promote numeric values (Int → Float when needed)
                let (l, r) = Expr::promote(left, right);
    
                match (l, r, op) {
                    (IntLiteral(a), IntLiteral(b), BinaryOp::Add) => Ok(IntLiteral(a + b)),
                    (IntLiteral(a), IntLiteral(b), BinaryOp::Subtract) => Ok(IntLiteral(a - b)),
                    (IntLiteral(a), IntLiteral(b), BinaryOp::Multiply) => Ok(IntLiteral(a * b)),
                    (IntLiteral(_), IntLiteral(0), BinaryOp::Divide) => Err("Divide by zero".into()),
                    (IntLiteral(a), IntLiteral(b), BinaryOp::Divide) => Ok(IntLiteral(a / b)),
    
                    (FloatLiteral(a), FloatLiteral(b), BinaryOp::Add) => Ok(FloatLiteral(a + b)),
                    (FloatLiteral(a), FloatLiteral(b), BinaryOp::Subtract) => Ok(FloatLiteral(a - b)),
                    (FloatLiteral(a), FloatLiteral(b), BinaryOp::Multiply) => Ok(FloatLiteral(a * b)),
                    (FloatLiteral(_), FloatLiteral(b), BinaryOp::Divide) if b == 0.0 =>
                        Err("Divide by zero".into()),
                    (FloatLiteral(a), FloatLiteral(b), BinaryOp::Divide) =>
                        Ok(FloatLiteral(a / b)),
    
                    _ => Err("Type error".into()),
                }
            }
    
            BinaryOp::Equal => {
                Ok(Bool(match (left, right) {
                    (IntLiteral(a), IntLiteral(b)) => a == b,
                    (FloatLiteral(a), FloatLiteral(b)) => a == b,
                    (Bool(a), Bool(b)) => a == b,
                    _ => false,
                }))
            }
    
            _ => Err("Unsupported op".into()),
        }
    }
    

    // Check if a value is truthy
    fn is_truthy(&self, value: &Value) -> bool {
        match value {
            Value::Bool(b) => *b,
            Value::IntLiteral(n) => *n != 0,
            Value::FloatLiteral(n) => *n != 0.0,
            _ => false,
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Let(String, Expr),
    Expr(Expr),
    Print(Expr),
    Return(Expr),
}