use crate::ast::ast::{Expr, Type, TypeEnv, TypeVarGenerator};
use std::collections::HashMap;

pub struct TypeInference 
{ 
    pub env: TypeEnv, 
    pub type_var_gen: TypeVarGenerator,
}

impl TypeInference
{ 
    pub fn new() -> Self 
    { 
        TypeInference 
        { 
            env: HashMap::new(), 
            type_var_gen: TypeVarGenerator::new(),
        }
    }

    pub fn infer(&mut self, expr: &Expr) -> Result<Type, String> 
    { 
        expr.infer_type(&mut self.env, &mut self.type_var_gen)
    }
}

impl Expr
{ 
    pub fn infer_type(&self, env: &mut TypeEnv, type_var_gen: &mut TypeVarGenerator) -> Result<Type, String>
    { 
        match self
        {
            Expr::Number(_) => Ok(Type::Float),
            Expr::Identifier(name) => 
            { 
                env.get(name)
                    .cloned() // TODO: implement polymorphic types with the TypeScheme object
                    .ok_or_else(|| format!("Undefined variable '{}'", name))
            }
            _ => Err("Not implemented!".into())
        }
    }
}