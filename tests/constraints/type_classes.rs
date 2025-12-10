use std::collections::HashMap;

use nexity::ast::{ast::{BinaryOp, Expr, Type}, types::{ClassDef, Instance, Substitution, TypeInference, solve_constraints}};

#[test]
fn test_eq_class_constraint_generation() {
    let mut type_infer = TypeInference::new();
    
    // Register Eq class
    type_infer.class_env.classes.insert(
        "Eq".to_string(),
        ClassDef {
            name: "Eq".to_string(),
            param: "a".to_string(),
            methods: HashMap::from([
                ("==".to_string(), Type::Function(
                    Box::new(Type::TypeVar("a".to_string())),
                    Box::new(Type::Function(
                        Box::new(Type::TypeVar("a".to_string())),
                        Box::new(Type::Bool)
                    ))
                ))
            ]),
        }
    );
    
    type_infer.class_env.instances.push(Instance {
        class_name: "Eq".to_string(),
        ty: Type::Int,
        methods: HashMap::new(),
    });
    
    let eq_lambda = Expr::Lambda(
        vec!["x".to_string(), "y".to_string()],
        Box::new(Expr::BinaryOp(
            Box::new(Expr::Identifier("x".to_string())),
            BinaryOp::Equal,
            Box::new(Expr::Identifier("y".to_string()))
        ))
    );
    
    let (ty, constraints) = type_infer.infer(&eq_lambda).unwrap();
    
    assert_eq!(constraints.len(), 1);
    assert_eq!(constraints[0].class, "Eq");
    
    println!("Lambda type: {:?}", ty);
    println!("Constraints: {:?}", constraints);
}

#[test]
fn test_constraint_solving() {
    let mut type_infer = TypeInference::new();
    
    type_infer.class_env.classes.insert(
        "Eq".to_string(),
        ClassDef {
            name: "Eq".to_string(),
            param: "a".to_string(),
            methods: HashMap::new(),
        }
    );
    
    type_infer.class_env.instances.push(Instance {
        class_name: "Eq".to_string(),
        ty: Type::Int,
        methods: HashMap::new(),
    });
    
    let eq_ints = Expr::BinaryOp(
        Box::new(Expr::Int(5)),
        BinaryOp::Equal,
        Box::new(Expr::Int(3))
    );
    
    let (ty, constraints) = type_infer.infer(&eq_ints).unwrap();
    
    let unsolved = solve_constraints(
        &constraints, 
        &type_infer.class_env, 
        &Substitution::new(),
        &type_infer.type_aliases
    ).unwrap();
    
    assert_eq!(ty, Type::Bool);
    assert_eq!(unsolved.len(), 0);
}