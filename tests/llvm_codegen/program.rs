use nexity::compile_program;

#[test]
fn test_simple_function() {
    let ir = compile_program("f x = x + 1").unwrap();
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
    assert!(ir.contains("define i64 @double"));
    assert!(ir.contains("define i64 @main"));
    assert!(ir.contains("call i64 @double"));
}

#[test]
fn test_multi_arg_function() {
    let ir = compile_program("
        add x y = x + y
        main = add 3 4
    ").unwrap();
    assert!(ir.contains("define i64 @add(i64 %x, i64 %y)"));
    assert!(ir.contains("call i64 @add(i64 3, i64 4)"));
}

#[test]
fn test_recursive_function() {
    let ir = compile_program("
        fact n = if n == 0 then 1 else n * fact (n - 1)
    ").unwrap();
    assert!(ir.contains("define i64 @fact"));
    assert!(ir.contains("call i64 @fact"));
}

#[test]
fn test_print_int() {
    let ir = compile_program("
        main = print 42
    ").unwrap();
    assert!(ir.contains("printf"));
    assert!(ir.contains("%lld"));
}

#[test]
fn test_print_float() {
    let ir = compile_program("
        main = print 3.14
    ").unwrap();
    assert!(ir.contains("printf"));
    assert!(ir.contains("%f"));
}

#[test]
fn test_pattern_match_literal() {
    let ir = compile_program("
        describe n = match n { 0 -> 1, 1 -> 2, _ -> 0 }
        main = print (describe 1)
    ").unwrap();
    assert!(ir.contains("define i64 @describe"));
    assert!(ir.contains("icmp eq"));
    assert!(ir.contains("phi"));
}

#[test]
fn test_where_clause() {
    let ir = compile_program("
        f x = x + y where y = 10
        main = print (f 5)
    ").unwrap();
    assert!(ir.contains("define i64 @f"));
}