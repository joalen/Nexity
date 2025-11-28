use nexity::ast::{ast::{Expr, Type}, types::TypeInference};

#[test]
fn test_basic_forall()
{ 
    let mut type_infer = TypeInference::new();
    
    // id :: forall a. a -> a
    let id_expr = Expr::Annotated(
        Box::new(Expr::Lambda(
            vec!["x".to_string()],
            Box::new(Expr::Identifier("x".to_string()))
        )),
        Type::Forall(
            vec!["a".to_string()],
            Box::new(Type::Function(
                Box::new(Type::TypeVar("a".to_string())),
                Box::new(Type::TypeVar("a".to_string()))
            ))
        )
    );
    
    match type_infer.infer(&id_expr) {
        Ok((ty, _)) => println!("Basic forall: {:?}", ty),
        Err(e) => println!("Basic forall failed: {}", e),
    }  
}

#[test]
fn test_rank2_application() {
    let mut type_infer = TypeInference::new();
    
    // apply :: (forall a. a -> a) -> Int -> Int
    // apply f x = f x
    let apply_expr = Expr::Lambda(
        vec!["f".to_string(), "x".to_string()],
        Box::new(Expr::Application(
            Box::new(Expr::Identifier("f".to_string())),
            Box::new(Expr::Identifier("x".to_string()))
        ))
    );
    
    match type_infer.infer(&apply_expr) {
        Ok((ty, _)) => println!("Rank-2 application: {:?}", ty),
        Err(e) => println!("Rank-2 application failed: {}", e),
    }
}


#[test]
fn test_runst_style() {
    let mut type_infer = TypeInference::new();
    
    // runST :: (forall s. ST s a) -> a
    // Simplified: \f -> f
    let runst = Expr::Annotated(
        Box::new(Expr::Lambda(
            vec!["f".to_string()],
            Box::new(Expr::Identifier("f".to_string()))
        )),
        Type::Function(
            Box::new(Type::Forall(
                vec!["s".to_string()],
                Box::new(Type::Function(
                    Box::new(Type::TypeVar("s".to_string())),
                    Box::new(Type::TypeVar("a".to_string()))
                ))
            )),
            Box::new(Type::TypeVar("a".to_string()))
        )
    );
    
    match type_infer.infer(&runst) {
        Ok((ty, _)) => println!("runST-style: {:?}", ty),
        Err(e) => println!("runST-style failed: {}", e),
    }
}