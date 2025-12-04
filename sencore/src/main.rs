use sencore::parser::Parser;

fn main() {
    let mut args = std::env::args();
    args.next().unwrap();
    let path = args.next().expect("Missing [PATH] argument");
    let source = std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("Failed to read file {:?}: {:?}", path, e));

    let mut parser = Parser::new(&source);
    let mut total = 0;
    while let Some(list) = parser.parse_list().unwrap() {
        println!("list: {:#?}", list);
        total += 1;
    }
    println!("total: {:?}", total);
}
