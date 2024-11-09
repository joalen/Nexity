use std::collections::HashMap;

// Define the `Value` type
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    Number(f64),
    Bool(bool),
    Function(Vec<String>, Box<Expr>, Env),  // Stores parameters, function body, and closure
}

// Define an environment type alias
pub type Env = HashMap<String, Value>;
// Define Binary operations
#[derive(Debug, PartialEq)]
pub enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

// Define Pattern
#[derive(Debug, PartialEq)]
pub enum Pattern {
    Literal(f64),
    Variable(String),
    Constructor(String, Vec<Pattern>),
    Wildcard,
}

// Implement evaluation for `Expr`
impl Expr {
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
            // Continue implementing cases as discussed...
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

    fn is_truthy(&self, value: &Value) -> bool {
        match value {
            Value::Bool(b) => *b,
            Value::Number(n) => *n != 0.0,
            _ => false,
        }
    }
}
