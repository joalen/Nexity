use std::collections::HashMap;

pub struct Interpreter 
{
    variables: HashMap<String, f64>,
}

impl Interpreter 
{
    pub fn new() -> Self 
    {
        Interpreter {
            variables: HashMap::new(),
        }
    }

    pub fn interpret(&mut self, stmt: &Stmt) -> Option<f64> 
    {
        match stmt 
        {
            Stmt::Let(name, expr) => 
            {
                let value = self.evaluate_expr(expr)?;
                self.variables.insert(name.clone(), value);
                Some(value)
            }
            Stmt::Expr(expr) => self.evaluate_expr(expr),
        }
    }

    fn evaluate_expr(&mut self, expr: &Expr) -> Option<f64> 
    {
        match expr 
        {
            Expr::Number(n) => Some(*n),
            Expr::Identifier(id) => self.variables.get(id).cloned(),
            Expr::BinaryOp(lhs, op, rhs) => 
            {
                let left = self.evaluate_expr
                let right = self.evaluate_expr(rhs)?;
                match op 
                {
                    BinaryOp::Add => Some(left + right),
                    BinaryOp::Subtract => Some(left - right),
                    BinaryOp::Multiply => Some(left * right),
                    BinaryOp::Divide => Some(left / right),
                }
            }
            Expr::If(cond, then_expr, else_expr) => 
            {
                let condition = self.evaluate_expr(cond)?;
                if condition != 0.0 
                {
                    self.evaluate_expr(then_expr)
                } else 
                {
                    self.evaluate_expr(else_expr)
                }
            }
        }
    }
}
