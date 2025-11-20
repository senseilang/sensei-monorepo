use bumpalo::Bump;
use neosen_parser::parse_via_tree_sitter;

fn main() {
    let mut args = std::env::args();
    args.next();
    let file_path = args.next().expect("Missing: [PATH]");

    println!("file_path: {:?}", file_path);

    let source = std::fs::read_to_string(&file_path).expect("Failed to read file");

    let arena = Bump::with_capacity(8000);
    let ast = parse_via_tree_sitter(&source, &arena);
}
