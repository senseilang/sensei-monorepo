use sencore::errors::format_parse_error;
use sencore::parser::{Parser, lower_sexpr_to_ast};

fn main() {
    let mut args = std::env::args();
    args.next().unwrap();
    let path = args.next().expect("Missing [PATH] argument");
    let source = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to read file {:?}: {:?}", path, e));

    let mut parser = Parser::new(&source);
    let list = match parser.parse_list() {
        Ok(Some(list)) => list,
        Ok(None) => {
            eprintln!("error: input does not contain a list");
            std::process::exit(1);
        }
        Err(e) => {
            eprintln!("{}", format_parse_error(&source, &e));
            std::process::exit(1);
        }
    };

    let ast = match lower_sexpr_to_ast(&list) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("{}", format_parse_error(&source, &e));
            std::process::exit(1);
        }
    };

    println!("{}", ast);

    sencore::interpreter::interpret(&ast.runtime_main);
}
