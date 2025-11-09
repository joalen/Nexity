use crate::ast::ast::BinaryOp;
use crate::ast::ast::{Expr, Type};
use std::collections::HashMap;
use std::collections::HashSet;

pub type TypeEnv = HashMap<String, TypeScheme>;
pub type Substitution = std::collections::HashMap<String, Type>;

#[derive(Debug, Clone)]
pub struct TypeScheme
{ 
    pub type_vars: Vec<String>,
    pub ty: Type,
}

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

pub fn instantiate(scheme: &TypeScheme, type_var_gen: &mut TypeVarGenerator) -> Type 
{ 
    let mut subst = Substitution::new();
    for var in &scheme.type_vars
    { 
        subst.insert(var.clone(), type_var_gen.fresh());
    }

    apply_substitution(&scheme.ty, &subst)
}

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
            Expr::Bool(_) => Ok(Type::Bool),
            Expr::String(_) => Ok(Type::Custom("String".to_string())),

            Expr::Identifier(name) => 
            { 
                let scheme = env.get(name).ok_or_else(|| format!("Unbound identifier: {}", name))?;
                Ok(instantiate(scheme, type_var_gen))
            }

            Expr::BinaryOp(lhs, op, rhs) =>
            { 
                let left_ty = lhs.infer_type(env, type_var_gen)?;
                let right_ty = rhs.infer_type(env, type_var_gen)?;
                
                match op
                { 
                    // arithmetic and numerical operations
                    BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide
                    | BinaryOp::Modulo => 
                    { 
                        if (left_ty == Type::Float && right_ty == Type::Float) // check for floats on both sides of expr
                        { 
                            Ok(Type::Float)
                        } else if (left_ty == Type::Int && right_ty == Type::Int) // check for ints on both sides of expr
                        { 
                            Ok(Type::Int)
                        } else 
                        { 
                            Err("Type error in binary operation. Must require numeric types on both sides".into())
                        }
                    }

                    // logic operations 
                    BinaryOp::And | BinaryOp::Or => 
                    { 
                        if (left_ty == Type::Bool && right_ty == Type::Bool)
                        { 
                            Ok(Type::Bool)
                        } else { 
                            Err("Logical operations require boolean types on both sides".into())
                        }
                    }

                    // equality and comparison 
                    BinaryOp::Equal | BinaryOp::NotEqual =>
                    { 
                        if (left_ty == right_ty)
                        { 
                            Ok(Type::Bool)
                        } else { 
                            Err("Equality operations require both sides to be of the same type".into())
                        }
                    }

                    BinaryOp::LessThan | BinaryOp::GreaterThan | BinaryOp::LessEqual | BinaryOp::GreaterEqual =>
                    { 
                        if (left_ty == Type::Float && right_ty == Type::Float) ||
                           (left_ty == Type::Int && right_ty == Type::Int)
                        { 
                            Ok(Type::Bool)
                        } else { 
                            Err("Comparison operations require numeric types on both sides".into())
                        }
                    }

                    _ => Err(format!("Unsupported binary operation: {:?}", op).into()),
                }
            }

            Expr::Lambda(params, body) =>
            { 
                let mut local_env = env.clone();
                let mut param_types = Vec::new();

                for param in params
                { 
                    let ty = type_var_gen.fresh();
                    local_env.insert(param.clone(), 
                        TypeScheme { type_vars: vec![], ty: ty.clone() });
                    
                    param_types.push(ty);
                }
                
                let body_ty = body.infer_type(&mut local_env, type_var_gen)?;
                let func_type = param_types.into_iter().rev().fold(body_ty, |acc, ty| {
                    Type::Function(Box::new(ty), Box::new(acc))
                });

                Ok(func_type)
            }

            _ => Err("Expression not supported".into()),
        }
    }
}

fn free_type_vars(ty: &Type) -> HashSet<String> {
    match ty {
        Type::TypeVar(name) => HashSet::from([name.clone()]),
        Type::Function(param, ret) => {
            let mut vars = free_type_vars(param);
            vars.extend(free_type_vars(ret));
            vars
        }
        _ => HashSet::new(),
    }
}

fn free_type_vars_in_env(env: &TypeEnv) -> HashSet<String> {
    env.values()
        .flat_map(|scheme| free_type_vars(&scheme.ty))
        .collect()
}

pub fn generalize(env: &TypeEnv, ty: &Type) -> TypeScheme {
    let env_vars = free_type_vars_in_env(env);
    let ty_vars = free_type_vars(ty);
    let type_vars: Vec<String> = ty_vars.difference(&env_vars).cloned().collect();
    TypeScheme { type_vars, ty: ty.clone() }
}
