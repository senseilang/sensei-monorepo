use bumpalo::Bump;

fn main() {
    let mut args = std::env::args();
    args.next();
    let file_path = args.next().expect("Missing: PATH");

    println!("file_path: {:?}", file_path);

    let _source = std::fs::read_to_string(&file_path).expect("Failed to read file");

    let _arena = Bump::with_capacity(8000);
    // let (ast, interner) = parse_via_tree_sitter(&source, &arena).expect("parse error");

    // println!("ast: {}", AstDisplay::new(&ast, &interner));
}
