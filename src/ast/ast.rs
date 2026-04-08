#![allow(dead_code)]

#[derive(Debug, Clone, PartialEq)]
pub enum Kind {
    Star,
    Arrow(Box<Kind>, Box<Kind>),
}

#[derive(Debug)]
pub enum Decl {
    Data(String, Vec<String>, Vec<Constructor>),
    TypeAlias(String, Vec<String>, Type),
    Class(String, String, Vec<MethodSig>),
    Instance(String, Type, Vec<MethodImpl>),
    TypeSig(String, Type),
    FuncDef(String, Vec<String>, Expr),
}

#[derive(Debug)]
pub struct MethodSig {
    pub name: String,
    pub ty: Type,
}

#[derive(Debug)]
pub struct MethodImpl {
    pub name: String,
    pub body: Expr,
}

#[derive(Debug)]
pub struct Constructor {
    pub name: String,
    pub fields: Vec<Type>,
    pub result_ty: Option<Type>,
    pub existential_vars: Vec<String>,
    pub existential_constraints: Vec<Constraint>,
}

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
    Apply(Box<Type>, Vec<Type>),
    Existential(Vec<String>, Vec<Constraint>, Box<Type>),
    Forall(Vec<String>, Box<Type>),
    Rigid(String),
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

#[derive(Debug, PartialEq, Clone)]
pub enum BinaryOp {
    Add, Subtract, Multiply, Divide, Modulo,
    And, Or,
    Equal, NotEqual,
    LessThan, GreaterThan, LessEqual, GreaterEqual,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Literal(f64),
    Variable(String),
    Constructor(String, Vec<Pattern>),
    Wildcard,
}

#[derive(Debug, PartialEq)]
pub enum Stmt {
    Let(String, Expr),
    Expr(Expr),
    Print(Expr),
    Return(Expr),
}