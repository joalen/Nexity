use std::collections::HashMap;

// things for my type-inferencing engine

pub type TypeEnv = HashMap<String, Type>; // TODO: will make this a TypeScheme to have polymorphic types (like for all a. a -> a)
pub struct TypeVarGenerator
{ 
    counter: usize,
}

impl TypeVarGenerator 
{ 
    pub fn new() -> Self 
    { 
        Self { counter: 0}
    }

    pub fn fresh(&mut self) -> Type 
    { 
        let name = format!("t{}", self.counter);
        self.counter += 1;
        Type::TypeVar(name)
    }
}

pub type Substitution = std::collections::HashMap<String, Type>;

// apply substitution to a type
pub fn apply_substitution(ty: &Type, subst: &Substitution) -> Type 
{ 
    match ty
    { 
        Type::TypeVar(name) => subst.get(name).cloned().unwrap_or(ty.clone()),
        Type::Function(param, ret) => 
        { 
            Type::Function(
                Box::new(apply_substitution(param, subst)),
                Box::new(apply_substitution(ret, subst)),
            )
        }

        _ => ty.clone(),
    }
}

// get the two substitutions 
pub fn compose_substitutions(s1: Substitution, s2: Substitution) -> Substitution
{ 
    let mut result = s2.into_iter() 
        .map(|(k, v) | (k, apply_substitution(&v, &s1)))
        .collect::<Substitution>();
    
    result.extend(s1);
    result
}

// checking to see if type variable occurs inside type (occurs check)
fn occurs_check(var: &str, ty: &Type) -> bool 
{ 
    match ty 
    { 
        Type::TypeVar(name) => name == var,
        Type::Function(param, ret) => occurs_check(var, param) || occurs_check(var, ret),
        _ => false,
    }
}

pub fn unify(t1: &Type, t2: &Type) -> Result<Substitution, String> 
{ 
    match (t1, t2)
    { 
        // primitives
        (Type::Int, Type::Int) | (Type::Float, Type::Float)
        | (Type::Bool, Type::Bool) | (Type::Char, Type::Char) => Ok(Substitution::new()),
    
        // type vars on either side
        (Type::TypeVar(name), t) | (t, Type::TypeVar(name)) =>
        { 
            if occurs_check(name, t)
            { 
                return Err(format!("Occurs check failed for type variable '{}'", name));
            }

            let mut subs = Substitution::new();
            subs.insert(name.clone(), t.clone());
            Ok(subs)
        }

        // function types
        (Type::Function(param1, ret1), Type::Function(param2, ret2)) => {
            let s1 = unify(param1, param2)?;
            let s2 = unify(&apply_substitution(ret1, &s1), &apply_substitution(ret2, &s1))?;
            Ok(compose_substitutions(s1, s2))
        }

        // can't unify
        _ => Err(format!("Cannot unify types {:?} and {:?}", t1, t2)),
    }
}


#[derive(Debug, PartialEq, Clone)]
pub enum Value {
    Number(f64),
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
}

#[derive(Debug, PartialEq, Clone)] 
pub enum Expr {
    Number(f64),
    Identifier(String),
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
            (Value::Number(n), Pattern::Literal(p)) => n == p,
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
            Expr::Number(n) => Ok(Value::Number(*n)),
            Expr::Bool(b) => Ok(Value::Bool(*b)),
            Expr::Identifier(name) => env.get(name)
                .cloned()
                .ok_or_else(|| format!("Undefined variable '{}'", name)),

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
                for (name, expr) in bindings {
                    let value = expr.evaluate(&mut local_env)?;
                    local_env.insert(name.clone(), value);
                }
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

            // future release
            /*Expr::Match(expr, cases) => {
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
            }*/



            _ => Err("Unsupported expression".into()),
        }
    }

    fn eval_binary_op(&self, left: Value, op: &BinaryOp, right: Value) -> Result<Value, String> {
        match (left, right, op) {
            (Value::Number(lhs), Value::Number(rhs), BinaryOp::Add) => Ok(Value::Number(lhs + rhs)),
            (Value::Number(lhs), Value::Number(rhs), BinaryOp::Subtract) => Ok(Value::Number(lhs - rhs)),
            (Value::Number(lhs), Value::Number(rhs), BinaryOp::Multiply) => Ok(Value::Number(lhs * rhs)),
            (Value::Number(lhs), Value::Number(rhs), BinaryOp::Divide) => {
                if rhs != 0.0 {
                    Ok(Value::Number(lhs / rhs))
                } else {
                    Err("Division by zero".into())
                }
            }
            _ => Err("Type error in binary operation".into()),
        }
    }

    // Check if a value is truthy
    fn is_truthy(&self, value: &Value) -> bool {
        match value {
            Value::Bool(b) => *b,
            Value::Number(n) => *n != 0.0,
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