use nexity::ast::{ast::{Constructor, Decl, Type}, types::TypeInference};

#[test]
fn test_kind_checking()
{
    let mut type_infer = TypeInference::new();

    // test 1 - Register Maybe a = Just a | Nothing
    let maybe_decl = Decl::Data(
        "Maybe".to_string(),
        vec!["a".to_string()],
        vec![
            Constructor { name: "Just".to_string(), fields: vec![Type::TypeVar("a".to_string())], result_ty: None, existential_vars: vec![], existential_constraints: vec![] },
            Constructor { name: "Nothing".to_string(), fields: vec![], result_ty: None, existential_vars: vec![], existential_constraints: vec![] }
        ]
    );
    type_infer.register_decl(&maybe_decl);

    // test 2 - valid - Maybe Int (kind checks out)
    let valid = Type::Apply(
        Box::new(Type::Custom("Maybe".to_string())),
        vec![Type::Int]
    );
    assert!(type_infer.check_kind(&valid, &type_infer.kind_env).is_ok());
    println!("Maybe Int is valid");

    // test 3 - invalid - Int Int (so we avoid this non-sense)
    let invalid = Type::Apply(
        Box::new(Type::Int),
        vec![Type::Int]
    );
    assert!(type_infer.check_kind(&invalid, &type_infer.kind_env).is_err());
    println!("Int Int rejected!");

    // test 4 - another invalid case - Maybe Int Int (too many type args)
    let too_many = Type::Apply(
        Box::new(Type::Custom("Maybe".to_string())),
        vec![Type::Int, Type::Int]
    );
    assert!(type_infer.check_kind(&too_many, &type_infer.kind_env).is_err());
    println!("Maybe Int Int rejected!");

    // test 5 - Either ab (kind * -> * -> *)
    let either_decl = Decl::Data(
        "Either".to_string(),
        vec!["a".to_string(), "b".to_string()],
        vec![
            Constructor { name: "Left".to_string(), fields: vec![Type::TypeVar("a".to_string())], result_ty: None, existential_vars: vec![], existential_constraints: vec![],},
            Constructor { name: "Right".to_string(), fields: vec![Type::TypeVar("b".to_string())], result_ty: None, existential_vars: vec![], existential_constraints: vec![],},
        ]
    );
    type_infer.register_decl(&either_decl);

    let either_int_bool = Type::Apply(
        Box::new(Type::Custom("Either".to_string())),
        vec![Type::Int, Type::Bool]
    );
    assert!(type_infer.check_kind(&either_int_bool, &type_infer.kind_env).is_ok());
    println!("Either Int Bool is valid");
}
