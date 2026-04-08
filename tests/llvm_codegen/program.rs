use nexity::compile_program;

#[test]
fn test_simple_function() {
    let ir = compile_program("f x = x + 1").unwrap();
    println!("{}", ir);
    assert!(ir.contains("define i64 @f"));
}

#[test]
fn test_two_functions() {
    let ir = compile_program("
        double x = x + x
        triple x = x + x + x
    ").unwrap();
    assert!(ir.contains("define i64 @double"));
    assert!(ir.contains("define i64 @triple"));
}

#[test]
fn test_function_with_if() {
    let ir = compile_program("abs x = if x < 0 then 0 else x").unwrap();
    println!("{}", ir);
    assert!(ir.contains("define i64 @abs"));
    assert!(ir.contains("icmp slt"));
    assert!(ir.contains("br i1"));
    assert!(ir.contains("phi i64"));
}

#[test]
fn test_function_call() {
    let ir = compile_program("
        double x = x + x
        main = double 21
    ").unwrap();
    println!("{}", ir);
    assert!(ir.contains("define i64 @double"));
    assert!(ir.contains("define i64 @main"));
    assert!(ir.contains("call i64 @double"));
}