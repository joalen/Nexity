use nexity::compile;

#[test]
fn test_e2e_int() {
    let ir = compile("42").unwrap();
    assert!(ir.contains("ret i64 42"));
}

#[test]
fn test_e2e_add() {
    let ir = compile("3 + 4").unwrap();
    assert!(ir.contains("ret i64 7"));
}

#[test]
fn test_e2e_if() {
    let ir = compile("if True then 1 else 0").unwrap();
    assert!(ir.contains("br i1"));
    assert!(ir.contains("phi"));
}