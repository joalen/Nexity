use nexity::ast::{ast::{Expr, Type}, types::{TypeInference, expand_type_alias}};

#[test]
fn test_simple_type_synonym() {
    let mut type_infer = TypeInference::new();
    
    // Register: type MyInt = Int
    type_infer.type_aliases.insert(
        "MyInt".to_string(),
        (vec![], Type::Int)
    );
    
    let expr = Expr::Annotated(
        Box::new(Expr::Int(42)),
        Type::Custom("MyInt".to_string())
    );
    
    let (ty, _) = type_infer.infer(&expr).unwrap();
    assert_eq!(ty, Type::Int);
}

#[test]
fn test_parameterized_type_synonym() {
    let mut type_infer = TypeInference::new();
    
    // Register: type Pair a b = (a, b)
    // For now just test the expansion logic since we don't have tuples
    type_infer.type_aliases.insert(
        "Pair".to_string(),
        (vec!["a".to_string(), "b".to_string()], 
         Type::Function(
             Box::new(Type::TypeVar("a".to_string())),
             Box::new(Type::TypeVar("b".to_string()))
         ))
    );
    
    let pair_int_bool = Type::Apply(
        Box::new(Type::Custom("Pair".to_string())),
        vec![Type::Int, Type::Bool]
    );
    
    let expanded = expand_type_alias(&pair_int_bool, &type_infer.type_aliases);
    assert_eq!(expanded, Type::Function(Box::new(Type::Int), Box::new(Type::Bool)));
}

#[test]
fn test_nested_type_synonym() {
    let mut type_infer = TypeInference::new();
    
    type_infer.type_aliases.insert("MyInt".to_string(), (vec![], Type::Int));
    type_infer.type_aliases.insert("MyMyInt".to_string(), (vec![], Type::Custom("MyInt".to_string())));
    
    let expanded = expand_type_alias(&Type::Custom("MyMyInt".to_string()), &type_infer.type_aliases);
    assert_eq!(expanded, Type::Int);
}

#[test]
fn test_partial_application()
{ 
    let mut type_infer = TypeInference::new();

    type_infer.type_aliases.insert(
        "Pair".to_string(), 
        (
            vec!["a".to_string(), "b".to_string()], 
            Type::Apply(
                Box::new(Type::Custom("Tuple2".to_string())),
                vec![Type::TypeVar("a".to_string()), Type::TypeVar("b".to_string())]
            )
        )
    );

    // Fully applied (sanity check): Pair Int String
    let pair_full = Type::Apply(
        Box::new(Type::Custom("Pair".to_string())),
        vec![Type::Int, Type::Custom("String".to_string())]
    );

    // Partially applied: Pair Int
    let pair_partial = Type::Apply(
        Box::new(Type::Custom("Pair".to_string())),
        vec![Type::Int]
    );

    // now expected expansions 
    let expected_full = Type::Apply(
        Box::new(Type::Custom("Tuple2".to_string())),
        vec![Type::Int, Type::Custom("String".to_string())]
    );

    let expected_partial = Type::Apply(
        Box::new(Type::Custom("Pair".to_string())),
        vec![Type::Int]
    );

    // fully applied should expand
    assert_eq!(
        expand_type_alias(&pair_full, &type_infer.type_aliases),
        expected_full,
        "Fully applied Pair should expand to Tuple2<Int, String>"
    );
    
    // partially applied should not
    assert_eq!(
        expand_type_alias(&pair_partial, &type_infer.type_aliases),
        expected_partial,
        "Partially applied Pair should not expand"
    );
}

#[test]
fn test_partial_type_synonyms() {
    let mut type_infer = TypeInference::new();
    
    // type Pair a b = (a, b)
    type_infer.type_aliases.insert(
        "Pair".to_string(),
        (vec!["a".to_string(), "b".to_string()],
         Type::Apply(
             Box::new(Type::Custom("Tuple2".to_string())),
             vec![Type::TypeVar("a".to_string()), Type::TypeVar("b".to_string())]
         ))
    );
    
    // type HalfPair a = Pair a
    type_infer.type_aliases.insert(
        "HalfPair".to_string(),
        (vec!["a".to_string()],
         Type::Apply(
             Box::new(Type::Custom("Pair".to_string())),
             vec![Type::TypeVar("a".to_string())]
         ))
    );
    
    // HalfPair Int String should expand to (Int, String)
    let half_pair_applied = Type::Apply(
        Box::new(Type::Custom("HalfPair".to_string())),
        vec![Type::Int, Type::Custom("String".to_string())]
    );
    
    let expanded = expand_type_alias(&half_pair_applied, &type_infer.type_aliases);
    println!("Expanded: {:?}", expanded);
    
    // Should be Tuple2<Int, String>
    assert_eq!(
        expanded,
        Type::Apply(
            Box::new(Type::Custom("Tuple2".to_string())),
            vec![Type::Int, Type::Custom("String".to_string())]
        )
    );
}