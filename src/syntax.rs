use std::fs;


fn check_aexp(code: String) -> bool {
    let mut result = false;

    // is it a number?
    match code.parse::<i32>() {
        Ok(_) => return true,
        Err(_) => (),
    }

    // is it a variable?
    let operators = ['+', '-', '*'];

    if !code.contains(" ") {
        if !code.contains("(") && !code.contains(")") {
            for opt in operators {
                if !code.contains(opt) {
                    result = true;
                }
                else {
                    result = false;
                    break;
                }
            }
        }
    }
    // parse the multiple contained expressions
    else {
        let mut open_bracket = 0;
        let mut closing_bracket = 0;
        let mut bracket_contents: Vec<String> = Vec::new();
        let mut content = String::from("");
        for c in code.chars() {
            content.push(c);
            if c == '(' {
                open_bracket += 1;
            }
            else if c == ')' {
                closing_bracket += 1;
            }
            else if operators.contains(&c) {
                if open_bracket == closing_bracket {
                    // remove operator
                    content.pop();
                    // remove trailing space
                    if content.ends_with(" ") { content.pop(); }
                    // remove leading and trailing bracket
                    if content.starts_with("(") & content.ends_with(")") {
                        content.remove(0);
                        content.pop();
                    }
                    else {
                        // leading and trailing cant occur alone
                        eprintln!("Wrong use of brackets within expression.");
                    }

                    bracket_contents.push(content);
                    content = String::from("");
                }
            }
        }
        if content != "" {
            bracket_contents.push(content);
        }
        for sub_expression in bracket_contents {
            if !check_aexp(sub_expression) {
                result = false;
            }
        }
    }
    return result;
}

fn check_bexp(code: String) -> bool {
    let mut result = false;

    // is it a constant? (True / False)
    match &code[..] {
        "True" => return true,
        "False" => return false,
        _ => (),
    }

    // check concatenated expressions (similar to aexp)
    let mut open_bracket = 0;
    let mut closing_bracket = 0;
    let mut aexp_contents: Vec<String> = Vec::new();
    let mut bexp_contents: Vec<String> = Vec::new();
    let mut collect_aexp = false;
    let mut content = String::from("");
    let a_operators = ["==", "<="];
    let b_operators = ["not", "and"];

    let characters: Vec<char> = code.chars().collect();
    let mut index: usize = 0;
    loop {
        if index >= characters.len() {
            break;
        }
        let c = characters[index];
        content.push(c);
        if c == '(' {
            open_bracket += 1;
            index += 1;
        }
        else if c == ')' {
            closing_bracket += 1;
            index += 1;
        }
        else if (index+2 < characters.len()) && (open_bracket == closing_bracket) {
            let mut intermediary_content = String::from("");
            intermediary_content.push(c);
            intermediary_content.push(characters[index+1]);
            if a_operators.contains(&&intermediary_content[..]) {
                content.pop();
                if content.ends_with(" ") { content.pop(); }
                aexp_contents.push(content);
                collect_aexp = true;
                content = "".to_string();
                index += 2;
            }
            else {
                intermediary_content.push(characters[index+2]);
                if b_operators.contains(&&intermediary_content[..]) {
                    content.pop();
                    if content.ends_with(" ") { content.pop(); }
                    bexp_contents.push(content);
                    collect_aexp = false;
                    content = "".to_string();
                    index += 3;
                }
            }
        }
    }
    if content != "" {
        if collect_aexp {
            aexp_contents.push(content);
        }
        else {
            bexp_contents.push(content);
        }
    }
    for aexp in aexp_contents {
        if !check_aexp(aexp) {
            result = false;
        }
    }
    for bexp in bexp_contents {
        if !check_bexp(bexp) {
            result = false;
        }
    }

    return result;
}

fn check_line_syntax(line: &String) -> bool {
    let mut result = false;

    if line.starts_with("if") {
        // if
        result = line.ends_with("then");
        if result {
            let bexp = line.replace("if ", "").replace(" then", "");
            result = check_bexp(bexp);
        }
    }
    else if line.starts_with("while") {
        // while
        result = line.ends_with("do");
        if result {
            let bexp = line.replace("while ", "").replace(" do", "");
            result = check_bexp(bexp);
        }
    }
    else if line == "skip;" {
        // skip
        result = true;
    }
    else {
        // assignment
        if !line.ends_with(";") {
            let acode: Vec<String> = line.split(" := ").map(|s| s.to_string()).collect();
            if acode.len() == 2 {
                let avar = &acode[0];
                let aexp = &acode[1];
                if avar.contains(" ") {
                    result = false;
                }
                else {
                    result = check_aexp(aexp.to_string());
                }
            }
            else {
                result = false;
            }
        }
        else {
            result = false;
        }
    }

    return result;
}

fn check_block_syntax(code: Vec<String>) -> bool {
    if code.len() == 0 {
        return false;
    }

    let mut subblocks: Vec<bool> = Vec::new();
    let mut result = false;
    let mut index = 0;

    loop {
        let line = &code[index];

        if line.starts_with("#") {
            // comment
            continue;
        }
        else if line.starts_with(" ") {
            // indentation with number of spaces not divisible by 4
            result = false;
            println!("Line \"{}\" has incorrect indentation", line);
            break;
        }
        else if line.starts_with("if") {
            result = check_line_syntax(line);
            if !result {
                break;
            }

            let mut leading = String::new();
            leading.push_str("    ");
            let mut block1: Vec<String> = Vec::new();
            let mut block2: Vec<String> = Vec::new();
            let mut found_else = false;

            loop {
                index = index + 1;
                if index == code.len() {
                    if !found_else {
                        println!("Incomplete if statement: Missing else");
                        result = false;
                    }
                    break;
                }
                let line = &code[index];
                if line.starts_with(" ") {
                    let line = line.replacen(&leading, "", 1);
                    if !found_else { block1.push(line); }
                    else { block2.push(line); }
                }
                else if line.starts_with("else") {
                    found_else = true;
                    continue;
                }
                else {
                    index = index - 1;
                    break;
                }
            }

            let res1 = check_block_syntax(block1);
            let res2 = check_block_syntax(block2);
            subblocks.push(res1);
            subblocks.push(res2);
        }
        else if line.starts_with("while") {
            result = check_line_syntax(line);
            if !result {
                break;
            }

            let mut leading = String::new();
            leading.push_str("    ");
            let mut block: Vec<String> = Vec::new();

            loop {
                index = index + 1;
                if index == code.len() {
                    break;
                }
                let line = &code[index];
                if line.starts_with(" ") {
                    let line = line.replacen(&leading, "", 1);
                    block.push(line);
                }
                else {
                    index = index - 1;
                    break;
                }
            }

            let res1 = check_block_syntax(block);
            subblocks.push(res1);
        }
        else {
            result = check_line_syntax(line);
            if !result {
                break;
            }
        }

        index = index + 1;
        if index == code.len() {
            break;
        }
    }
    for block in subblocks {
        result = block;
        if !result {
            break;
        }
    }
    return result;
}

pub fn check_file_syntax(path: &String) -> bool {
    let code = fs::read_to_string(&path).expect("File could not be read");
    let code: Vec<String> = code.split("\n").map(|s| s.to_string()).collect();

    return check_block_syntax(code);
}