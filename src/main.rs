use std::env;
use std::fs;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: nexity <file>");
        std::process::exit(1);
    }

    let source = fs::read_to_string(&args[1])
        .unwrap_or_else(|e| { eprintln!("Error reading file: {}", e); std::process::exit(1); });

    let output = std::path::Path::new(&args[1])
        .file_stem()
        .unwrap()
        .to_str()
        .unwrap()
        .to_string();

    match nexity::compile_to_binary(&source, &output) {
        Ok(()) => println!("Compiled to ./{}", output),
        Err(e) => eprintln!("Error: {}", e),
    }
}