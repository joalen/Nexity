#[derive(Debug, PartialEq)]
pub enum Type {
    Int,
    Float,
    Bool,
    Char,
    Function(Box<Type>, Box<Type>),
    Custom(String),
    TypeVar(String),
}

pub enum Expr 
{
    Number(f64),
    Identifier(String),
    BinaryOp(Box<Expr>, BinaryOp, Box<Expr>),
    If(Box<Expr>, Box<Expr>, Box<Expr>),
    Function(String, Box<Expr>),
    Lambda(Vec<String>, Box<Expr>),
    Application(Box<Expr>, Box<Expr>),
    Match(Box<Expr>, Vec<(Pattern, Option<Expr>, Expr)>),
    Let(Vec<(String, Expr)>, Box<Expr>),
    Where(Box<Expr>, Vec<(String, Expr)>),
    Bool(bool),
    Not(Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
}

#[derive(Debug, PartialEq)]
pub enum BinaryOp 
{
    Add,
    Subtract,
    Multiply,
    Divide,
    And, 
    Or,
    Equal,
    NotEqual,
    LessThan,
    GreaterThan,
    LessEqual,
    GreaterEqual,
}

pub enum Pattern 
{
    Literal(f64),
    Variable(String),
    Constructor(String, Vec<Pattern>),
    Wildcard,
}

#[derive(Debug, PartialEq)]
pub enum Stmt 
{
    Let(String, Expr),
    Expr(Expr),
}
