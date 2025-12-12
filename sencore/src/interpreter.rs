use crate::ast;
use crate::comptime_value::*;

pub fn interpret(create_runtime: &ast::Expr) {
    let mut types = Types::new();
    let mut bindings = Env::default();
    std::rc::Rc;
}
