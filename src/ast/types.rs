use crate::ast::ast::{BinaryOp, Constraint, Decl, Kind, Pattern};
use crate::ast::ast::{Expr, Type};
use std::collections::HashMap;
use std::collections::HashSet;
use std::vec;

pub type KindEnv = HashMap<String, Kind>;
pub type TypeEnv = HashMap<String, TypeScheme>;
pub type Substitution = std::collections::HashMap<String, Type>;

pub struct ClassEnv {
    pub classes: HashMap<String, ClassDef>,
    pub instances: Vec<Instance>,
}

#[derive(Debug, Clone)]
pub struct ClassDef {
    pub name: String,
    pub param: String,
    pub methods: HashMap<String, Type>,
}

#[derive(Debug, Clone)]
pub struct Instance {
    pub class_name: String,
    pub ty: Type,
    pub methods: HashMap<String, Expr>,
}

pub struct FuncClause {
    pub patterns: Vec<Pattern>,
    pub body: Expr,
}

pub struct FuncDecl
{
    pub name: String,
    pub clauses: Vec<FuncClause>,
    pub variants: Vec<Variant>
}

// ADTs
pub struct TypeDecl
{ 
    pub name: String, 
    pub type_params: Vec<String>,
    pub variants: Vec<Variant>
}

pub struct Variant
{ 
    pub name: String,
    pub arg_types: Vec<Type>,
    pub result_ty: Option<Type>,
    pub existential_vars: Vec<String>,
    pub existential_constraints: Vec<Constraint>, 
}

pub enum Item 
{
    Func(FuncDecl),
    Type(TypeDecl),
}

#[derive(Debug, Clone)]
pub struct TypeScheme
{ 
    pub type_vars: Vec<String>,
    pub constraints: Vec<Constraint>,
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

        Type::Existential(vars, constraints, body) => {
            let filtered_subst: Substitution = subst
                .iter()
                .filter(|(k, _)| !vars.contains(k))
                .map(|(k, v)| (k.clone(), v.clone()))
                .collect();
            
            Type::Existential(vars.clone(), constraints.clone(), Box::new(apply_substitution(body, &filtered_subst)))
        }

        Type::Forall(vars, body) => 
        { 
            let filtered_subst: Substitution = subst
                .iter() 
                .filter(|(k, _) | !vars.contains(k))
                .map(|(k, v) | (k.clone(), v.clone()))
                .collect();
            
            Type::Forall(vars.clone(), Box::new(apply_substitution(body, &filtered_subst)))
        }

        Type::Apply(ctor, args) => {
            Type::Apply(
                Box::new(apply_substitution(ctor, subst)),
                args.iter().map(|arg| apply_substitution(arg, subst)).collect()
            )
        }

        Type::Constrained(constraints, inner) => {
            Type::Constrained(constraints.clone(), Box::new(apply_substitution(inner, subst)))
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

pub fn unify(t1: &Type, t2: &Type, aliases: &HashMap<String, (Vec<String>, Type)>) -> Result<Substitution, String> 
{ 
    match (t1, t2)
    { 
        // primitives
        (Type::Int, Type::Int) | (Type::Float, Type::Float)
        | (Type::Bool, Type::Bool) | (Type::Char, Type::Char) => Ok(Substitution::new()),
    
        // type vars on either side
        (Type::TypeVar(name), t) | (t, Type::TypeVar(name)) =>
        { 
            if let Type::TypeVar(other_name) = t {
                if name == other_name {
                    return Ok(Substitution::new()); // Same var, nothing to do
                }
            }
            
            if occurs_check(name, t) {
                return Err(format!("Occurs check failed for type variable '{}'", name));
            }
            let mut subs = Substitution::new();
            subs.insert(name.clone(), t.clone());
            Ok(subs)
        }

        // function types
        (Type::Function(param1, ret1), Type::Function(param2, ret2)) => {
            let s1 = unify(&expand_type_alias(param1, aliases), &expand_type_alias(param2, aliases), aliases)?;
            let s2 = unify(&apply_substitution(ret1, &s1), &apply_substitution(ret2, &s1), aliases)?;
            Ok(compose_substitutions(s1, s2))
        }

        // Existential unpacking: ∃a. T becomes a fresh type variable
        (Type::Existential(vars, constraints, body), ty) | (ty, Type::Existential(vars, constraints, body)) => {
            let mut subst = Substitution::new();
            let mut generator = TypeVarGenerator::new();
            for v in vars {
                subst.insert(v.clone(), generator.fresh());
            }
            let instantiated = apply_substitution(body, &subst);
            unify(&instantiated, ty, aliases)
        }

        // forall types 
        (Type::Forall(vars, body), ty) | (ty, Type::Forall(vars, body)) => {
            // Instantiate the forall type with fresh variables
            let mut subst = Substitution::new();
            let mut generator = TypeVarGenerator::new();
            for v in vars {
                subst.insert(v.clone(), generator.fresh());
            }
            let instantiated = apply_substitution(body, &subst);
            unify(&instantiated, ty, aliases)
        }

        // applying types 
        (Type::Apply(ctor1, args1), Type::Apply(ctor2, args2)) => {
            let s1 = unify(ctor1, ctor2, aliases)?;
            
            if args1.len() != args2.len() {
                return Err("Type application arity mismatch".into());
            }
            
            let mut subst = s1;
            for (a1, a2) in args1.iter().zip(args2.iter()) {
                let s = unify(
                    &apply_substitution(a1, &subst),
                    &apply_substitution(a2, &subst),
                    aliases
                )?;
                subst = compose_substitutions(s, subst);
            }
            Ok(subst)
        }

        // rigid types cannot unify with anything else rather than themselves
        (Type::Rigid(name1), Type::Rigid(name2)) if name1 == name2 => Ok(Substitution::new()),
        (Type::Rigid(name), _) | (_, Type::Rigid(name)) => {
            Err(format!("Cannot unify rigid existential variable '{}'", name))
        }
        
        // custom types
        (Type::Custom(name1), Type::Custom(name2)) if name1 == name2 => Ok(Substitution::new()),

        // can't unify
        _ => Err(format!("Cannot unify types {:?} and {:?}", t1, t2)),
    }
}

pub struct TypeInference 
{ 
    pub env: TypeEnv, 
    pub type_var_gen: TypeVarGenerator,
    pub class_env: ClassEnv,
    pub adt_env: HashMap<String, TypeDecl>,
    pub kind_env: KindEnv,
    pub type_aliases: HashMap<String, (Vec<String>, Type)>,
    pub signatures: HashMap<String, Type>
}

impl TypeInference
{ 
    pub fn new() -> Self 
    { 
        // checking for kinds
        let mut kind_env: KindEnv = HashMap::new();
        kind_env.insert("Int".to_string(), Kind::Star);
        kind_env.insert("Float".to_string(), Kind::Star);
        kind_env.insert("Bool".to_string(), Kind::Star);
        kind_env.insert("Char".to_string(), Kind::Star);

        TypeInference 
        { 
            env: HashMap::new(), 
            type_var_gen: TypeVarGenerator::new(),
            class_env: ClassEnv {
                classes: HashMap::new(),
                instances: Vec::new(),
            },
            adt_env: HashMap::new(),
            kind_env,
            type_aliases: HashMap::new(),
            signatures: HashMap::new(),
        }
    }

    pub fn infer(&mut self, expr: &Expr) -> Result<(Type, Vec<Constraint>), String> 
    { 
        expr.infer_type(&mut self.env, &mut self.type_var_gen, &self.adt_env, &self.type_aliases)
    }

    pub fn check_kind(&self, ty: &Type, kind_env: &KindEnv) -> Result<Kind, String> 
    { 
        match ty
        { 
            Type::Int | Type::Bool | Type::Float | Type::Char => Ok(Kind::Star),

            Type::Custom(name) => 
            { 
                kind_env.get(name)
                    .cloned()
                    .ok_or_else(|| format!("Unknown type constructor: {}", name))
            }

            Type::Apply(constructor, args) => 
            { 
                let ctor_kind = self.check_kind(constructor, kind_env)?;
                let mut current_kind = ctor_kind;

                for arg in args
                { 
                    match current_kind 
                    { 
                        Kind::Arrow(param_kind, ret_kind) => 
                        { 
                            let arg_kind = self.check_kind(arg, kind_env)?;
                            if *param_kind != arg_kind 
                            { 
                                return Err("Kind mismatch!".into());
                            }
                            current_kind = *ret_kind;
                        }

                        Kind::Star => return Err("Too many type arguments".into())
                    }
                }

                Ok(current_kind)
            }

            _ => Ok(Kind::Star)
        }
    }

    pub fn register_decl(&mut self, decl: &Decl)
    { 
        match decl
        { 
            Decl::TypeSig(name, ty) => {
                self.signatures.insert(name.clone(), ty.clone());
            }

            Decl::FuncDef(name, params, body) => {
                let lambda = if params.is_empty() {
                    body.clone()
                } else {
                    Expr::Lambda(params.clone(), Box::new(body.clone()))
                };
                
                // Infer the actual type
                let (inferred_ty, constraints) = lambda.infer_type(
                    &mut self.env,
                    &mut self.type_var_gen,
                    &self.adt_env,
                    &self.type_aliases
                ).expect("Type inference failed");
                
                // If we have a signature, check it matches
                if let Some(sig_ty) = self.signatures.get(name).cloned() {
                    println!("Signature: {:?}", sig_ty);
                    println!("Inferred: {:?}", inferred_ty);

                    match unify(&sig_ty, &inferred_ty, &self.type_aliases) {
                        Ok(subst) => {
                            let final_ty = apply_substitution(&sig_ty, &subst);
                            let scheme = generalize(&self.env, &final_ty, constraints);
                            self.env.insert(name.clone(), scheme);
                        }
                        Err(e) => {
                            panic!("Function doesn't match its type signature: {:?}", e);
                        }
                    }
                } else {
                    // No signature, just use inferred type
                    let scheme = generalize(&self.env, &inferred_ty, constraints);
                    self.env.insert(name.clone(), scheme);
                }
            }

            Decl::Data(name, type_params, constructors) => 
            { 
                let kind = type_params.iter().rev().fold(Kind::Star, |acc, _| {
                    Kind::Arrow(Box::new(Kind::Star), Box::new(acc))
                });

                self.kind_env.insert(name.clone(), kind);

                self.adt_env.insert(name.clone(), TypeDecl
                { 
                    name: name.clone(),
                    type_params: type_params.clone(),
                    variants: constructors.iter().map(|c| Variant {
                        name: c.name.clone(),
                        arg_types: c.fields.clone(),
                        result_ty: c.result_ty.clone(), // Pass through GADT result type
                        existential_vars: c.existential_vars.clone(),
                        existential_constraints: c.existential_constraints.clone(),
                    }).collect(),
                });
            }

            Decl::TypeAlias(name, params, ty) => 
            { 
                self.type_aliases.insert(name.clone(), (params.clone(), ty.clone()));
            }

            _ => {} // handling other decls later
        }
    }
}

impl Expr
{ 
    pub fn infer_type(&self, env: &mut TypeEnv, type_var_gen: &mut TypeVarGenerator, adt_env: &HashMap<String, TypeDecl>, type_aliases: &HashMap<String, (Vec<String>,Type)>) -> Result<(Type, Vec<Constraint>), String> { 
    match self
    {
        Expr::Int(_) => Ok((Type::Int, vec![])),
        Expr::Float(_) => Ok((Type::Float, vec![])),
        Expr::Bool(_) => Ok((Type::Bool, vec![])),
        Expr::String(_) => Ok((Type::Custom("String".to_string()), vec![])),

        Expr::Not(expr) => {
            let (ty, constraints) = expr.infer_type(env, type_var_gen, adt_env, type_aliases)?;
            unify(&ty, &Type::Bool, type_aliases)?;
            Ok((Type::Bool, constraints))
        }

        Expr::Identifier(name) => 
        { 
            // check if we're at a constructor first 
            if name.chars().next().unwrap().is_uppercase()
            { 
                // Look up in ADT env for constructor types
                for (_, type_decl) in adt_env.iter()
                { 
                    for variant in &type_decl.variants
                    { 
                        if variant.name == *name
                        { 
                            // build out the constructor type: field1 -> field2 -> ... -> ResultType
                            let result_ty = if type_decl.type_params.is_empty()
                            { 
                                Type::Custom(type_decl.name.clone())
                            } else { 
                                Type::Apply
                                (
                                    Box::new(Type::Custom(type_decl.name.clone())),
                                    type_decl.type_params.iter().map(|p| Type::TypeVar(p.clone())).collect()
                                )
                            };

                            let ctor_ty = variant.arg_types.iter().rfold(result_ty, |acc, field| {
                                Type::Function(Box::new(field.clone()), Box::new(acc))
                            });

                            return Ok((ctor_ty, variant.existential_constraints.clone()));
                        }
                    }
                }
            }


            let scheme = env.get(name)
                .ok_or_else(|| format!("Unbound identifier: {}", name))?;
            let mut ty = instantiate(scheme, type_var_gen);
            ty = expand_type_alias(&ty, type_aliases);
            Ok((ty, scheme.constraints.clone()))
        }

        Expr::BinaryOp(lhs, op, rhs) =>
        { 
            let (left_ty, mut left_constraints) = lhs.infer_type(env, type_var_gen, adt_env, type_aliases)?;
            let (right_ty, right_constraints) = rhs.infer_type(env, type_var_gen, adt_env, type_aliases)?;
            left_constraints.extend(right_constraints);
            
            match op
            { 
                BinaryOp::Add | BinaryOp::Subtract | BinaryOp::Multiply | BinaryOp::Divide
                | BinaryOp::Modulo => 
                { 
                    unify(&left_ty, &right_ty, type_aliases)?;
                    
                    let result_ty = match (&left_ty, &right_ty) {
                        (Type::Float, Type::Float) => Type::Float,
                        (Type::Int, Type::Int) => Type::Int,
                        (Type::TypeVar(_), Type::Float) | (Type::Float, Type::TypeVar(_)) => Type::Float,
                        (Type::TypeVar(_), Type::Int) | (Type::Int, Type::TypeVar(_)) => Type::Int,
                        (Type::TypeVar(_), Type::TypeVar(_)) => Type::Float,
                        _ => return Err("Arithmetic operations require numeric types".into())
                    };
                    
                    Ok((result_ty, left_constraints))
                }

                BinaryOp::And | BinaryOp::Or => 
                { 
                    unify(&left_ty, &Type::Bool, type_aliases)?;
                    unify(&right_ty, &Type::Bool, type_aliases)?;
                    Ok((Type::Bool, left_constraints))
                }

                BinaryOp::Equal | BinaryOp::NotEqual =>
                { 
                    unify(&left_ty, &right_ty, type_aliases)?;
                    
                    left_constraints.push(Constraint {
                        class: "Eq".to_string(),
                        ty: Box::new(left_ty),
                    });
                    
                    Ok((Type::Bool, left_constraints))
                }

                BinaryOp::LessThan | BinaryOp::GreaterThan | 
                BinaryOp::LessEqual | BinaryOp::GreaterEqual =>
                { 
                    unify(&left_ty, &right_ty, type_aliases)?;
                    
                    match (&left_ty, &right_ty) {
                        (Type::Float, Type::Float) | (Type::Int, Type::Int) |
                        (Type::TypeVar(_), Type::Float) | (Type::Float, Type::TypeVar(_)) |
                        (Type::TypeVar(_), Type::Int) | (Type::Int, Type::TypeVar(_)) |
                        (Type::TypeVar(_), Type::TypeVar(_)) => Ok((Type::Bool, left_constraints)),
                        _ => Err("Comparison requires numeric types".into())
                    }
                }
            }
        }

        Expr::Lambda(params, body) =>
        { 
            let mut param_types = Vec::new();
        
            for param in params
            { 
                let ty = type_var_gen.fresh();
                env.insert(param.clone(), 
                    TypeScheme { type_vars: vec![], constraints: vec![], ty: ty.clone() });
                param_types.push(ty);
            }
            
            let (body_ty, body_constraints) = body.infer_type(env, type_var_gen, adt_env, type_aliases)?;
            
            for param in params {
                env.remove(param);
            }
            
            let func_type = param_types.into_iter().rev().fold(body_ty, |acc, ty| {
                Type::Function(Box::new(ty), Box::new(acc))
            });
        
            Ok((func_type, body_constraints))
        }

        Expr::Let(bindings, body) => {
            let mut placeholders: HashMap<String, Type> = HashMap::new();
            let mut all_constraints = Vec::new();
            
            for (name, _) in bindings.iter() {
                let tv = type_var_gen.fresh();
                placeholders.insert(name.clone(), tv);
            }
        
            let mut local_env = env.clone();
            for (name, tv) in placeholders.iter() {
                local_env.insert(
                    name.clone(),
                    TypeScheme { type_vars: vec![], constraints: vec![], ty: tv.clone() },
                );
            }
        
            let mut subst: Substitution = Substitution::new();
            for (name, expr) in bindings.iter() {
                let (inferred_ty, constraints) = expr.infer_type(&mut local_env, type_var_gen, adt_env, type_aliases)?;
                all_constraints.extend(constraints);
                
                let tv = placeholders.get(name).unwrap();
                let s = unify(tv, &inferred_ty, type_aliases)?;
                subst = compose_substitutions(s, subst);
        
                for (_, scheme) in local_env.iter_mut() {
                    scheme.ty = apply_substitution(&scheme.ty, &subst);
                }
            }
        
            for (name, _) in bindings.iter() {
                let final_ty = apply_substitution(placeholders.get(name).unwrap(), &subst);
                let scheme = generalize(&local_env, &final_ty, all_constraints.clone());
                local_env.insert(name.clone(), scheme.clone());
            }
        
            for (name, _) in bindings {
                env.insert(name.clone(), local_env.get(name).unwrap().clone());
            }
            
            let (body_ty, body_constraints) = body.infer_type(&mut local_env, type_var_gen, adt_env, type_aliases)?;
            all_constraints.extend(body_constraints);
            
            Ok((apply_substitution(&body_ty, &subst), all_constraints))
        }

        Expr::If(cond, then_branch, else_branch) => 
        {
            let (cond_ty, mut cond_constraints) = cond.infer_type(env, type_var_gen, adt_env, type_aliases)?;
            if cond_ty != Type::Bool {
                return Err("Condition in if-expression must be Bool".into());
            }
        
            let (then_ty, then_constraints) = then_branch.infer_type(env, type_var_gen, adt_env, type_aliases)?;
            let (else_ty, else_constraints) = else_branch.infer_type(env, type_var_gen, adt_env, type_aliases)?;
        
            unify(&then_ty, &else_ty, type_aliases)?;
            
            cond_constraints.extend(then_constraints);
            cond_constraints.extend(else_constraints);
            
            Ok((then_ty, cond_constraints))
        }

        Expr::Application(func, arg) => {
            let (func_ty, mut func_constraints) = func.infer_type(env, type_var_gen, adt_env, type_aliases)?;
            let (arg_ty, arg_constraints) = arg.infer_type(env, type_var_gen, adt_env, type_aliases)?;
            func_constraints.extend(arg_constraints);
            
            let result_ty = type_var_gen.fresh();
            let func_expected_ty = Type::Function(Box::new(arg_ty), Box::new(result_ty.clone()));
            let subst = unify(&func_ty, &func_expected_ty, type_aliases)?;

            let mut result_constraints = func_constraints.clone();
            result_constraints = result_constraints.iter().map(|c| Constraint {
                class: c.class.clone(),
                ty: Box::new(apply_substitution(&c.ty, &subst)),
            }).collect();            
            
            Ok((apply_substitution(&result_ty, &subst), result_constraints))
        }

        Expr::Match(scrutinee, arms) => 
        { 
            let (scrutinee_ty, mut all_constraints) = scrutinee.infer_type(env, type_var_gen, adt_env, type_aliases)?;
            let mut arm_tys = Vec::new();

            for (pattern, guard, body) in arms 
            { 
                let (pat_ty, bindings, pat_constraints) = infer_pattern(pattern, type_var_gen, adt_env);
                all_constraints.extend(pat_constraints);

                let subst = unify(&scrutinee_ty, &pat_ty, type_aliases)?;
                let mut local_env = env.clone();

                for (_, scheme) in local_env.iter_mut() {
                    scheme.ty = apply_substitution(&scheme.ty, &subst);
                }

                for (name, ty) in bindings
                { 
                    let refined_ty = apply_substitution(&ty, &subst);
                    let scheme = generalize(&local_env, &refined_ty, vec![]);
                    local_env.insert(name, scheme);
                }
        
                if let Some(g) = guard 
                { 
                    let (guard_ty, guard_constraints) = g.infer_type(&mut local_env, type_var_gen, adt_env, type_aliases)?;
                    unify(&guard_ty, &Type::Bool, type_aliases)?;
                    all_constraints.extend(guard_constraints);
                }
        
                let (body_ty, body_constraints) = body.infer_type(&mut local_env, type_var_gen, adt_env, type_aliases)?;
                all_constraints.extend(body_constraints);

                arm_tys.push(apply_substitution(&body_ty, &subst));
            }

            let first = arm_tys[0].clone();
            for t in arm_tys.iter().skip(1) {
                unify(&first, t, type_aliases)?;
            }
        
            Ok((first, all_constraints))
        }

        Expr::Annotated(expr, ann_ty) => {
            let (inferred_ty, constraints) = expr.infer_type(env, type_var_gen, adt_env, type_aliases)?;
            let expanded_ann = expand_type_alias(ann_ty, type_aliases);
            let subst = unify(&inferred_ty, &expanded_ann, type_aliases)?;
            Ok((apply_substitution(&expanded_ann, &subst), constraints))
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

        Type::Existential(vars, _constraints, body) => {
            let mut free = free_type_vars(body);
            for v in vars {
                free.remove(v);
            }
            free
        }

        Type::Forall(vars, body) => {
            let mut free = free_type_vars(body);
            for v in vars {
                free.remove(v);
            }
            free
        }

        _ => HashSet::new(),
    }
}

fn free_type_vars_in_env(env: &TypeEnv) -> HashSet<String> {
    env.values()
        .flat_map(|scheme| free_type_vars(&scheme.ty))
        .collect()
}

pub fn infer_pattern(pattern: &Pattern, type_var_gen: &mut TypeVarGenerator, adt_env: &HashMap<String, TypeDecl>) -> (Type, Vec<(String, Type)>, Vec<Constraint>) 
{
    match pattern {
        Pattern::Literal(n) => {
            if n.fract() == 0.0 {
                (Type::Int, vec![], vec![])
            } else {
                (Type::Float, vec![], vec![])
            }
        }

        Pattern::Variable(name) => {
            let tv = type_var_gen.fresh();
            (tv.clone(), vec![(name.clone(), tv)], vec![])
        }

        Pattern::Wildcard => {
            (type_var_gen.fresh(), vec![], vec![])
        }

        Pattern::Constructor(name, subpatterns) => {
            let mut bindings = vec![];
            let mut constraints = vec![];

            for type_decl in adt_env.values() { // get constructor from env
                for variant in &type_decl.variants {
                    if variant.name == *name {
                        if variant.arg_types.len() != subpatterns.len() {
                            panic!("Constructor arity mismatch for {}", name); // confirm arity matched
                        }
                        
                        // freshen existential vars
                        let mut subst = Substitution::new();
                        for ex_var in &variant.existential_vars {
                            // using Rigid instead of fresh TypeVar
                            let rigid_var = Type::Rigid(format!("ex_{}", type_var_gen.counter));
                            type_var_gen.counter += 1;  // manually increment
                            subst.insert(ex_var.clone(), rigid_var);
                        }

                        for constraint in &variant.existential_constraints {
                            let instantiated_constraint = Constraint {
                                class: constraint.class.clone(),
                                ty: Box::new(apply_substitution(&constraint.ty, &subst)),
                            };
                            constraints.push(instantiated_constraint);
                        }

                        // apply to field types (for freshened existentials)
                        for (i, pat) in subpatterns.iter().enumerate() {
                            let field_ty = apply_substitution(&variant.arg_types[i], &subst);
                            
                            // Match on pattern type to handle Variable specially
                            match pat {
                                Pattern::Variable(var_name) => {
                                    // Bind variable to FRESHENED (opaque) field type
                                    bindings.push((var_name.clone(), field_ty));
                                }
                                _ => {
                                    let (pat_ty, pat_bindings, pat_constraints) = infer_pattern(pat, type_var_gen, adt_env);
                                    bindings.extend(pat_bindings);
                                    constraints.extend(pat_constraints);
                                    // Could unify pat_ty with field_ty here
                                }
                            }
                        }
                        
                        // Build result type
                        // GADT: use explicit result type if present
                        let result_ty = if let Some(ref gadt_result) = variant.result_ty {
                            apply_substitution(gadt_result, &subst)
                        } else { // Normal ADT: build result type
                            if type_decl.type_params.is_empty() {
                                Type::Custom(type_decl.name.clone())
                            } else {
                                Type::Apply(
                                    Box::new(Type::Custom(type_decl.name.clone())),
                                    type_decl.type_params.iter()
                                        .map(|_| type_var_gen.fresh())
                                        .collect()
                                )
                            }
                        };
                        
                        return (result_ty, bindings, constraints);
                    }
                }
            }
            
            panic!("Unknown constructor: {}", name);
        }
    }
}

pub fn generalize(env: &TypeEnv, ty: &Type, constraints: Vec<Constraint>) -> TypeScheme {
    let env_vars = free_type_vars_in_env(env);
    let ty_vars = free_type_vars(ty);
    let type_vars: Vec<String> = ty_vars.difference(&env_vars).cloned().collect();
    TypeScheme { type_vars, constraints, ty: ty.clone() }
}

pub fn expand_type_alias(ty: &Type, aliases: &HashMap<String, (Vec<String>, Type)>) -> Type {
    expand_type_alias_guarded(ty, aliases, 0, 10)
}

pub fn solve_constraints(
    constraints: &[Constraint],
    class_env: &ClassEnv,
    subst: &Substitution,
    type_aliases: &HashMap<String, (Vec<String>, Type)>
) -> Result<Vec<Constraint>, String> {
    let mut unsolved = Vec::new();
    
    for constraint in constraints {
        let ty = apply_substitution(&constraint.ty, subst);
        
        // Check if there's a matching instance
        let mut found = false;
        for inst in &class_env.instances {
            if inst.class_name == constraint.class {
                match unify(&ty, &inst.ty, type_aliases) {
                    Ok(_) => {
                        found = true;
                        break;
                    }
                    Err(_) => continue,
                }
            }
        }
        
        if !found {
            unsolved.push(Constraint {
                class: constraint.class.clone(),
                ty: Box::new(ty),
            });
        }
    }
    
    Ok(unsolved)
}

pub fn expand_type_alias_guarded(
    ty: &Type, 
    aliases: &HashMap<String, (Vec<String>, Type)>,
    depth: usize,
    max_depth: usize
) -> Type {
    if depth > max_depth {
        return ty.clone();
    }

    match ty {
        Type::Custom(name) => {
            if let Some((params, body)) = aliases.get(name) {
                if params.is_empty() {
                    match body {
                        Type::Apply(_, _) => ty.clone(),
                        _ => expand_type_alias_guarded(body, aliases, depth + 1, max_depth)
                    }
                } else {
                    ty.clone() // Has params but not applied - keep as is
                }
            } else {
                ty.clone()
            }
        }

        Type::Existential(vars, constraints, body) => Type::Existential(
            vars.clone(),
            constraints.clone(),
            Box::new(expand_type_alias_guarded(body, aliases, depth, max_depth))
        ),
        
        Type::Apply(constructor, args) => {
            if let Type::Custom(name) = &**constructor {
                if let Some((params, body)) = aliases.get(name) {
                    let num_params = params.len();
                    let num_args = args.len();
                    
                    if num_args >= num_params {
                        let mut subst = HashMap::new();
                        for (param, arg) in params.iter().zip(args.iter()) {
                            subst.insert(param.clone(), arg.clone());
                        }
                        
                        let expanded_body = apply_substitution(body, &subst);
                        let remaining_args: Vec<Type> = args[num_params..].to_vec();
                    
                        // If there are remaining args, merge them with the expanded body
                        let to_expand = if !remaining_args.is_empty() {
                            match expanded_body {
                                Type::Apply(ctor, mut inner_args) => {
                                    // Merge: Pair [Int] + [String] = Pair [Int, String]
                                    inner_args.extend(remaining_args);
                                    Type::Apply(ctor, inner_args)
                                }
                                Type::Custom(name) => {
                                    // Direct constructor: Pair + [String] = Pair [String]
                                    Type::Apply(Box::new(Type::Custom(name)), remaining_args)
                                }
                                other => {
                                    // Can't apply to non-constructor
                                    Type::Apply(Box::new(other), remaining_args)
                                }
                            }
                        } else {
                            expanded_body
                        };
                    
                        // Now expand the result
                        return expand_type_alias_guarded(&to_expand, aliases, depth + 1, max_depth);
                    } else {
                        // Partially applied - create lambda abstraction
                        return Type::Apply(
                            constructor.clone(),
                            args.iter().map(|a| expand_type_alias_guarded(a, aliases, depth, max_depth)).collect()
                        );
                    }
                }
            }
            
            Type::Apply(
                Box::new(expand_type_alias_guarded(constructor, aliases, depth, max_depth)),
                args.iter().map(|a| expand_type_alias_guarded(a, aliases, depth, max_depth)).collect()
            )
        }

        Type::Function(a, b) => Type::Function(
            Box::new(expand_type_alias_guarded(a, aliases, depth, max_depth)),
            Box::new(expand_type_alias_guarded(b, aliases, depth, max_depth))
        ),
        
        _ => ty.clone()
    }
}