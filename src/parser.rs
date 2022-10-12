use std::fs;


fn parse_code(code: Vec<String>) -> String {
    if code.len() == 0 {
        return "Skip".to_string();
    }

    let mut statements: Vec<String> = Vec::new();
    let mut program = String::new();
    let mut index = 0;

    loop {
        let line = &code[index];

        if line.starts_with("#") {
            // comment
            continue;
        }
        else if line.starts_with("if") {
            // parse bexp
            // parse corresponding indented block
            let bexp = String::new();
            let block1 = String::new();
            let block2 = String::new();

            let sub_program = format!("If ({}) ({}) ({})", bexp, block1, block2);
            statements.push(sub_program.to_string());
        }

        index += 1;
        if index >= code.len() {
            break;
        }
    }

    for statement in statements {
        program = format!("{};{}", program, statement);
    }

    return program;
}

pub fn parse_file(path: &String) -> String {
    let code = fs::read_to_string(&path).expect("File could not be read");
    let code: Vec<String> = code.split("\n").map(|s| s.to_string()).collect();

    return parse_code(code);
}