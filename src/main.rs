use std::env;
use std::path::Path;
use std::process::exit;

mod syntax;
mod parser;


fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() == 1 {
        eprintln!("Not enough arguments");
        exit(1);
    }

    let source_path = &args[1];
    println!("Parsing file {}", source_path);
    if !Path::new(&source_path).exists() {
        eprintln!("File does not exist");
        exit(1);
    }
    let success = syntax::check_file_syntax(source_path);

    if success {
        println!("File: Ok");
        let parsed_code = parser::parse_file(source_path);
        println!("{}", parsed_code)
    }
    else {
        println!("File: Invalid");
    }
}
