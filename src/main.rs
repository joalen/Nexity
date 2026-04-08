mod lexer;
mod parser;
mod llvmgen;
mod ast; 

fn main() {
    let source = "
        fact n = if n == 0 then 1 else n * fact (n - 1)
        main = fact 10
    ";

    match nexity::compile_to_binary(source, "output") {
        Ok(()) => println!("Compiled to ./output"),
        Err(e) => eprintln!("Error: {}", e),
    }
}