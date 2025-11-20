use crate::ast::ast::{BinaryOp, Pattern};
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
            Expr::Int(_) => Ok(Type::Int),
            Expr::Float(_) => Ok(Type::Float),
            Expr::Bool(_) => Ok(Type::Bool),
            Expr::String(_) => Ok(Type::Custom("String".to_string())),

            Expr::Identifier(name) => 
            { 
                println!("Looking up '{}', env keys: {:?}", name, env.keys());
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
                        unify(&left_ty, &right_ty)?; // unify operands together
                        let numeric_ty = type_var_gen.fresh(); // fresh var for result

                        match (&left_ty, &right_ty) {
                            (Type::Float, Type::Float) => Ok(Type::Float),
                            (Type::Int, Type::Int) => Ok(Type::Int),
                            (Type::TypeVar(_), Type::Float) | (Type::Float, Type::TypeVar(_)) => Ok(Type::Float),
                            (Type::TypeVar(_), Type::Int) | (Type::Int, Type::TypeVar(_)) => Ok(Type::Int),
                            (Type::TypeVar(_), Type::TypeVar(_)) => {
                                // Both are type vars - default to Float
                                Ok(Type::Float)
                            }
                            _ => Err("Arithmetic operations require numeric types on both sides".into())
                        }
                    }

                    // logic operations 
                    BinaryOp::And | BinaryOp::Or => 
                    { 
                        // Unify both with Bool type
                        unify(&left_ty, &Type::Bool)?;
                        unify(&right_ty, &Type::Bool)?;
                        Ok(Type::Bool)
                    }

                    // equality and comparison 
                    BinaryOp::Equal | BinaryOp::NotEqual =>
                    { 
                        unify(&left_ty, &right_ty)?;
                        Ok(Type::Bool)
                    }

                    BinaryOp::LessThan | BinaryOp::GreaterThan | BinaryOp::LessEqual | BinaryOp::GreaterEqual =>
                    { 
                        unify(&left_ty, &right_ty)?;

                        match (&left_ty, &right_ty) {
                            (Type::Float, Type::Float) => Ok(Type::Bool),
                            (Type::Int, Type::Int) => Ok(Type::Bool),
                            (Type::TypeVar(_), Type::Float) | (Type::Float, Type::TypeVar(_)) => Ok(Type::Bool),
                            (Type::TypeVar(_), Type::Int) | (Type::Int, Type::TypeVar(_)) => Ok(Type::Bool),
                            (Type::TypeVar(_), Type::TypeVar(_)) => Ok(Type::Bool),
                            _ => Err("Comparison operations require numeric types on both sides".into())
                        }
                    }

                    _ => Err(format!("Unsupported binary operation: {:?}", op).into()),
                }
            }

            Expr::Lambda(params, body) =>
            { 
                let mut param_types = Vec::new();
            
                for param in params
                { 
                    let ty = type_var_gen.fresh();
                    env.insert(param.clone(), 
                        TypeScheme { type_vars: vec![], ty: ty.clone() });
                    
                    param_types.push(ty);
                }
                
                let body_ty = body.infer_type(env, type_var_gen)?;
                
                // Clean up parameters from env
                for param in params {
                    env.remove(param);
                }
                
                let func_type = param_types.into_iter().rev().fold(body_ty, |acc, ty| {
                    Type::Function(Box::new(ty), Box::new(acc))
                });
            
                Ok(func_type)
            }

            Expr::Let(bindings, body) => {
                // Step 1: Create fresh type variables for each binding
                let mut placeholders: HashMap<String, Type> = HashMap::new();
                for (name, _) in bindings.iter() {
                    let tv = type_var_gen.fresh();
                    placeholders.insert(name.clone(), tv);
                }
            
                // Step 2: Extend the environment with placeholders for mutual recursion
                let mut local_env = env.clone();
                for (name, tv) in placeholders.iter() {
                    local_env.insert(
                        name.clone(),
                        TypeScheme { type_vars: vec![], ty: tv.clone() },
                    );
                }
            
                // Step 3: Infer the type of each RHS under the extended environment
                let mut subst: Substitution = Substitution::new();
                for (name, expr) in bindings.iter() {
                    let inferred_ty = expr.infer_type(&mut local_env, type_var_gen)?;
                    let tv = placeholders.get(name).unwrap();
                    let s = unify(tv, &inferred_ty)?;
                    subst = compose_substitutions(s, subst);
            
                    // Apply substitution to all types in the environment
                    for (_, scheme) in local_env.iter_mut() {
                        scheme.ty = apply_substitution(&scheme.ty, &subst);
                    }
                }
            
                // Step 4: Generalize each binding and update the outer environment
                for (name, _) in bindings.iter() {
                    let final_ty = apply_substitution(placeholders.get(name).unwrap(), &subst);
                    let scheme = generalize(&local_env, &final_ty);
                    local_env.insert(name.clone(), scheme.clone());
                }
            
                for (name, _) in bindings {
                    env.insert(name.clone(), local_env.get(name).unwrap().clone());
                }
                
                // Step 5: Infer the body type
                let body_ty = body.infer_type(&mut local_env, type_var_gen)?;
                Ok(apply_substitution(&body_ty, &subst))
            }

            Expr::If(cond, then_branch, else_branch) => 
            {
                let cond_ty = cond.infer_type(env, type_var_gen)?;
                if cond_ty != Type::Bool {
                    return Err("Condition in if-expression must be Bool".into());
                }
            
                let then_ty = then_branch.infer_type(env, type_var_gen)?;
                let else_ty = else_branch.infer_type(env, type_var_gen)?;
            
                unify(&then_ty, &else_ty)?;
                Ok(then_ty)
            }

            Expr::Application(func, arg) => {
                let func_ty = func.infer_type(env, type_var_gen)?;
                let arg_ty = arg.infer_type(env, type_var_gen)?;
                let result_ty = type_var_gen.fresh();
                
                let func_expected_ty = Type::Function(Box::new(arg_ty.clone()), Box::new(result_ty.clone()));
            
                // Perform unification and store the resulting substitution
                let subst = unify(&func_ty, &func_expected_ty)?;
                
                // Apply the substitution to func_ty (or other types that were unified)
                let func_ty_after = apply_substitution(&func_ty, &subst);
                let arg_ty_after = apply_substitution(&arg_ty, &subst);
            
                Ok(apply_substitution(&result_ty, &subst))
            }

            Expr::Match(scrutinee, arms) => 
            { 
                let scrutinee_ty = scrutinee.infer_type(env, type_var_gen)?;
                let mut arm_tys = Vec::new();

                for (pattern, guard, body) in arms 
                { 
                    let (par_ty, bindings) = infer_pattern(pattern, type_var_gen);
                    unify(&scrutinee_ty, &par_ty)?;

                    // extend to environment with generalized pattern bindings 
                    let mut local_env = env.clone();

                    for (name, ty) in bindings
                    { 
                        let scheme = generalize(env, &ty);
                        local_env.insert(name, scheme);
                    }

                    // optional guards check
                    if let Some(g) = guard 
                    { 
                        let guard_ty = g.infer_type(&mut local_env, type_var_gen)?;
                        unify(&guard_ty, &Type::Bool)?;
                    }

                    let body_ty = body.infer_type(&mut local_env, type_var_gen)?;
                    arm_tys.push(body_ty);
                }

                // now unify all types
                let first = arm_tys[0].clone();
                for t in arm_tys.iter().skip(1) {
                    unify(&first, t)?;
                }
            
                Ok(first)
            }

            Expr::Annotated(expr, ann_ty) => {
                let inferred_ty = expr.infer_type(env, type_var_gen)?;
                let subst = unify(&inferred_ty, ann_ty)?;
                Ok(apply_substitution(ann_ty, &subst))
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

pub fn infer_pattern(pattern: &Pattern, type_var_gen: &mut TypeVarGenerator) -> (Type, Vec<(String, Type)>) 
{
    match pattern {
        Pattern::Literal(n) => {
            if n.fract() == 0.0 {
                (Type::Int, vec![])
            } else {
                (Type::Float, vec![])
            }
        }

        Pattern::Variable(name) => {
            let tv = type_var_gen.fresh();
            (tv.clone(), vec![(name.clone(), tv)])
        }

        Pattern::Wildcard => {
            (type_var_gen.fresh(), vec![])
        }

        Pattern::Constructor(_, subpatterns) => {
            // If you add ADTs later, fill this in properly
            let mut bindings = vec![];
            let mut arg_tys = vec![];

            for pat in subpatterns {
                let (ty, pat_bindings) = infer_pattern(pat, type_var_gen);
                bindings.extend(pat_bindings);
                arg_tys.push(ty);
            }

            // constructor type unknown now → fresh type, unify later
            let result_ty = type_var_gen.fresh();
            let fn_ty = arg_tys.into_iter().rfold(result_ty.clone(), |ret, param| {
                Type::Function(Box::new(param), Box::new(ret))
            });

            (fn_ty, bindings)
        }
    }
}

pub fn generalize(env: &TypeEnv, ty: &Type) -> TypeScheme {
    let env_vars = free_type_vars_in_env(env);
    let ty_vars = free_type_vars(ty);
    let type_vars: Vec<String> = ty_vars.difference(&env_vars).cloned().collect();
    TypeScheme { type_vars, ty: ty.clone() }
}
