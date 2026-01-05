use crate::ast::*;
use crate::comptime_value::{Builtin, Value};
use crate::lexer::{Lexer, Token};

use crate::span::Span;

#[derive(Debug, Clone)]
pub struct SNode<'src> {
    pub kind: SNodeKind<'src>,
    pub span: Span<usize>,
}

impl<'src> SNode<'src> {
    pub fn new(kind: SNodeKind<'src>, span: Span<usize>) -> Self {
        Self { kind, span }
    }
}

#[derive(Debug, Clone)]
pub enum SNodeKind<'src> {
    Num(i32),
    Bool(bool),
    Name(&'src str),
    List(Vec<SNode<'src>>),
}

impl<'src> SNodeKind<'src> {
    fn describe(&self) -> String {
        match self {
            SNodeKind::Num(n) => format!("number `{}`", n),
            SNodeKind::Bool(b) => format!("boolean `{}`", b),
            SNodeKind::Name(name) => format!("identifier `{}`", name),
            SNodeKind::List(items) => {
                if items.is_empty() {
                    "empty list `()`".to_string()
                } else if let SNodeKind::Name(name) = &items[0].kind {
                    format!("list starting with `{}`", name)
                } else {
                    "nested list".to_string()
                }
            }
        }
    }
}

pub struct Parser<'src> {
    source: &'src str,
    tokens: std::iter::Peekable<Lexer<'src>>,
}

macro_rules! snode {
    ($name:ident, $span:expr) => {
        SNode::new(SNodeKind::$name, $span)
    };
    ($name:ident, $value:expr, $span:expr) => {
        SNode::new(SNodeKind::$name($value), $span)
    };
}

#[derive(Debug, Clone)]
pub struct ParseError {
    pub message: String,
    pub span: Span<usize>,
}

pub struct LoweringCtx {
    next_struct_id: u32,
}

impl LoweringCtx {
    pub fn new() -> Self {
        Self { next_struct_id: 0 }
    }

    fn alloc_struct_id(&mut self) -> u32 {
        let id = self.next_struct_id;
        self.next_struct_id += 1;
        id
    }
}

impl Default for LoweringCtx {
    fn default() -> Self {
        Self::new()
    }
}

fn expect_name<'src>(node: &SNode<'src>) -> Result<Name, ParseError> {
    match &node.kind {
        SNodeKind::Name(name) => Ok(Name {
            name: (*name).into(),
            span: node.span,
        }),
        _ => Err(ParseError {
            message: format!("Expected name, got {:?}", node.kind),
            span: node.span,
        }),
    }
}

fn expect_list<'src, 'a>(node: &'a SNode<'src>) -> Result<&'a [SNode<'src>], ParseError> {
    match &node.kind {
        SNodeKind::List(list) => Ok(list.as_slice()),
        _ => Err(ParseError {
            message: format!("Expected list, got {:?}", node.kind),
            span: node.span,
        }),
    }
}

fn is_comptime_keyword(node: &SNode) -> bool {
    matches!(&node.kind, SNodeKind::Name("comptime"))
}

impl<'src> Parser<'src> {
    pub fn new(source: &'src str) -> Self {
        Self {
            source,
            tokens: Lexer::new(source).peekable(),
        }
    }

    pub fn parse_list(&mut self) -> Result<Option<SNode<'src>>, ParseError> {
        let eof_span = Span::new(self.source.len(), self.source.len());

        let tok = self.tokens.next();
        let start_span = match tok {
            Some((Token::RoundOpen, start_span)) => start_span,
            Some((ty, span)) => {
                return Err(ParseError {
                    message: format!("Unexpected token {:?}, expected '(' or EOF", ty),
                    span,
                });
            }
            None => return Ok(None),
        };

        let mut children = Vec::new();
        let mut end_span;

        loop {
            let (ty, span) = *self.tokens.peek().ok_or_else(|| ParseError {
                message: format!("Unexpected EOF, expected <list_item> or EOF"),
                span: eof_span,
            })?;
            end_span = span;
            match ty {
                Token::True => {
                    self.tokens.next();
                    children.push(snode!(Bool, true, span));
                }
                Token::False => {
                    self.tokens.next();
                    children.push(snode!(Bool, false, span));
                }
                Token::RoundClose => {
                    self.tokens.next();
                    break;
                }
                Token::Ident(name) => {
                    self.tokens.next();
                    children.push(snode!(Name, name, span));
                }
                Token::Num(num) => {
                    self.tokens.next();
                    let num = num.parse().expect("lexer didn't validate num");
                    children.push(snode!(Num, num, span));
                }
                Token::RoundOpen => {
                    children.push(
                        self.parse_list()?
                            .expect("expected Some(list)/error got: None"),
                    );
                }
                Token::Error => {
                    return Err(ParseError {
                        message: format!(
                            "Invalid character sequence: {:?}",
                            &self.source[span.range()]
                        ),
                        span,
                    });
                }
            }
        }

        Ok(Some(snode!(
            List,
            children,
            Span::new(start_span.start, end_span.end)
        )))
    }
}

/// (e1 e2 ... eN) => left-fold into FuncApp: ((e1 e2) e3) ... eN
fn lower_apply(
    ctx: &mut LoweringCtx,
    span: Span<usize>,
    args: &[SNode],
) -> Result<Expr, ParseError> {
    let (first, remaining_args) = args.split_first().ok_or_else(|| ParseError {
        message: format!("apply requires at least 1 argument, got 0"),
        span,
    })?;

    let first = snode_to_expr(ctx, first)?;
    remaining_args.iter().try_fold(first, |acc, node| {
        let arg = snode_to_expr(ctx, node)?;
        Ok(Expr {
            span: Span::new(acc.span.start, arg.span.end),
            kind: ExprKind::FuncApp(Box::new(FuncApp {
                func_expr: acc,
                applying_expr: arg,
            })),
        })
    })
}

/// (funcdef comptime? <bind:name> <type:expr> <body:expr>)
fn lower_func(
    ctx: &mut LoweringCtx,
    span: Span<usize>,
    args: &[SNode],
) -> Result<Expr, ParseError> {
    // Check if first argument is "comptime" keyword
    let (is_comptime, remaining_args) = match args {
        [first, rest @ ..] if is_comptime_keyword(first) => (true, rest),
        _ => (false, args),
    };

    let [bind, type_expr, body] = remaining_args else {
        return Err(ParseError {
            message: format!(
                "func requires exactly 3 arguments (bind, type, body), got {}",
                remaining_args.len()
            ),
            span,
        });
    };

    let func_bind = expect_name(bind)?;
    let bind_type_expr = snode_to_expr(ctx, type_expr)?;
    let body = snode_to_expr(ctx, body)?;

    Ok(Expr {
        span,
        kind: ExprKind::FuncDef(Box::new(FuncDef {
            recursive_name: None,
            is_comptime,
            func_bind,
            bind_type_expr,
            body,
        })),
    })
}

/// (recfuncdef <funcref:name> comptime? <bind:name> <type:expr> <body:expr>)
fn lower_recursive_func(
    ctx: &mut LoweringCtx,
    span: Span<usize>,
    args: &[SNode],
) -> Result<Expr, ParseError> {
    let (funcref, is_comptime, bind, r#type, body) = match args {
        [funcref, comptime_keyword, bind, r#type, body] => {
            if !is_comptime_keyword(comptime_keyword) {
                return Err(ParseError {
                    message: format!("Expected `comptime` keyword, got: {:?}", comptime_keyword),
                    span,
                });
            }
            (funcref, true, bind, r#type, body)
        }
        [funcref, bind, r#type, body] => (funcref, false, bind, r#type, body),
        args => {
            return Err(ParseError {
                message: format!(
                    "recursive function definition expects 4 or 5 arguments, got: {}",
                    args.len()
                ),
                span,
            });
        }
    };

    let funcref = expect_name(funcref)?;
    let func_bind = expect_name(bind)?;
    let bind_type_expr = snode_to_expr(ctx, r#type)?;
    let body = snode_to_expr(ctx, body)?;

    Ok(Expr {
        span,
        kind: ExprKind::FuncDef(Box::new(FuncDef {
            recursive_name: Some(funcref),
            is_comptime,
            func_bind,
            bind_type_expr,
            body,
        })),
    })
}

/// (if <condition> <true_branch> <false_branch>)
fn lower_if(ctx: &mut LoweringCtx, span: Span<usize>, args: &[SNode]) -> Result<Expr, ParseError> {
    let [cond, then_branch, else_branch] = args else {
        return Err(ParseError {
            message: format!(
                "if requires exactly 3 arguments (condition, then, else), got {}",
                args.len()
            ),
            span,
        });
    };

    let condition = snode_to_expr(ctx, cond)?;
    let true_branch = snode_to_expr(ctx, then_branch)?;
    let false_branch = snode_to_expr(ctx, else_branch)?;

    Ok(Expr {
        span,
        kind: ExprKind::IfThenElse(Box::new(IfThenElse {
            condition,
            true_branch,
            false_branch,
        })),
    })
}

/// (block (<comptime>? <name> <type> <value>)* <result>)
/// Desugars into nested lambda applications:
/// (block (x T1 e1) (y T2 e2) e3) => ((λ x:T1. ((λ y:T2. e3) e2)) e1)
fn lower_block(
    ctx: &mut LoweringCtx,
    span: Span<usize>,
    args: &[SNode],
) -> Result<Expr, ParseError> {
    let (end_node, let_nodes) = args.split_last().ok_or_else(|| ParseError {
        message: "block requires at least a result expression".into(),
        span,
    })?;

    let lets = let_nodes
        .iter()
        .map(|node| lower_let_bind(ctx, node))
        .collect::<Result<Vec<_>, _>>()?;
    let end_expr = snode_to_expr(ctx, end_node).map_err(|mut err| {
        err.message = format!("[block expr] {}", err.message);
        err
    })?;

    // Desugar: fold right over let bindings
    // For each binding, wrap the result in (λ name:type. result) value
    let result = lets.into_iter().rev().fold(end_expr, |body, let_bind| {
        let func_span = Span::new(let_bind.span.start, body.span.end);
        let func_def = Expr {
            span: func_span,
            kind: ExprKind::FuncDef(Box::new(FuncDef {
                recursive_name: None,
                is_comptime: let_bind.is_comptime,
                func_bind: let_bind.bind_local,
                bind_type_expr: let_bind.bind_type_expr,
                body,
            })),
        };
        let app_span = Span::new(let_bind.span.start, let_bind.assigned.span.end);
        Expr {
            span: app_span,
            kind: ExprKind::FuncApp(Box::new(FuncApp {
                func_expr: func_def,
                applying_expr: let_bind.assigned,
            })),
        }
    });

    Ok(result)
}

/// (attr <path_segment:name>+ <struct_expr>)
/// Left-fold: (attr a b expr) => MemberAccess(MemberAccess(expr, a), b)
fn lower_attr(
    ctx: &mut LoweringCtx,
    span: Span<usize>,
    args: &[SNode],
) -> Result<Expr, ParseError> {
    if args.len() < 2 {
        return Err(ParseError {
            message: format!(
                "attr requires at least 2 arguments (path segment(s) and struct expr), got {}",
                args.len()
            ),
            span,
        });
    }

    let (struct_node, path_segments) = args.split_last().unwrap();
    let mut acc = snode_to_expr(ctx, struct_node)?;

    for seg_node in path_segments {
        let member = expect_name(seg_node)?;
        acc = Expr {
            span: Span::new(acc.span.start, member.span.end),
            kind: ExprKind::MemberAccess(Box::new(MemberAccess {
                r#struct: acc,
                member,
            })),
        };
    }

    Ok(acc)
}

/// (struct_def capture (<name> <type>)*)
fn lower_struct(
    ctx: &mut LoweringCtx,
    span: Span<usize>,
    args: &[SNode],
) -> Result<Expr, ParseError> {
    let [capture, fields @ ..] = args else {
        return Err(ParseError {
            message: format!(
                "struct requires at least 1 argument (capture, fields*), got {}",
                args.len()
            ),
            span,
        });
    };

    let capture = snode_to_expr(ctx, capture)?;

    let fields = fields
        .iter()
        .map(|field_node| {
            let field_components = expect_list(field_node)?;
            let [name_node, type_node] = field_components else {
                return Err(ParseError {
                    message: format!(
                        "field must have exactly 2 elements (name, type), got {}",
                        field_components.len()
                    ),
                    span: field_node.span,
                });
            };
            let name = expect_name(name_node)?;
            let r#type = snode_to_expr(ctx, type_node)?;
            Ok(StructField {
                span: field_node.span,
                name,
                r#type,
            })
        })
        .collect::<Result<_, _>>()?;

    Ok(Expr {
        span,
        kind: ExprKind::StructDef(Box::new(StructDef {
            def_uuid: ctx.alloc_struct_id(),
            capture,
            fields,
        })),
    })
}

/// (struct_init <struct_type:expr> (<field:name> <value:expr>)*)
fn lower_struct_init(
    ctx: &mut LoweringCtx,
    span: Span<usize>,
    args: &[SNode],
) -> Result<Expr, ParseError> {
    let (type_node, field_nodes) = args.split_first().ok_or_else(|| ParseError {
        message: "struct_init requires at least a struct type argument".into(),
        span,
    })?;

    let struct_type = snode_to_expr(ctx, type_node)?;

    let mut fields = Vec::with_capacity(field_nodes.len());
    for field_node in field_nodes {
        let field_list = expect_list(field_node)?;
        let [name_node, value_node] = field_list else {
            return Err(ParseError {
                message: format!(
                    "struct_init field must have exactly 2 elements (name, value), got {}",
                    field_list.len()
                ),
                span: field_node.span,
            });
        };

        let name = expect_name(name_node)?;
        let value = snode_to_expr(ctx, value_node)?;
        fields.push(StructInitField {
            span: field_node.span,
            name,
            value,
        });
    }

    Ok(Expr {
        span,
        kind: ExprKind::StructInit(Box::new(StructInit {
            struct_type,
            fields,
        })),
    })
}

fn snode_to_expr(ctx: &mut LoweringCtx, node: &SNode) -> Result<Expr, ParseError> {
    let expr = |kind| {
        Ok(Expr {
            span: node.span,
            kind,
        })
    };
    let value = |v| expr(ExprKind::Value(Box::new(v)));

    let list = match &node.kind {
        SNodeKind::Num(x) => return value(Value::Num(*x)),
        SNodeKind::Bool(b) => return value(Value::Bool(*b)),
        SNodeKind::Name(name) => return expr(ExprKind::Var((*name).into())),
        SNodeKind::List(list) => list,
    };
    let [first, args @ ..] = list.as_slice() else {
        return value(Value::Void);
    };

    let &SNodeKind::Name(name) = &first.kind else {
        if args.is_empty() {
            return snode_to_expr(ctx, first);
        } else {
            return lower_apply(ctx, node.span, list);
        }
    };

    let e = match name {
        "funcdef" => lower_func(ctx, node.span, args)?,
        "recfuncdef" => lower_recursive_func(ctx, node.span, args)?,
        "if" => lower_if(ctx, node.span, args)?,
        "block" => lower_block(ctx, node.span, args)?,
        "attr" => lower_attr(ctx, node.span, args)?,
        "struct_def" => lower_struct(ctx, node.span, args)?,
        "struct_init" => lower_struct_init(ctx, node.span, args)?,
        "meta__struct_get_field" => lower_builtin(ctx, args, Builtin::GetStructField, node.span)?,
        "meta__is_struct" => lower_builtin(ctx, args, Builtin::IsStruct, node.span)?,
        "meta__struct_get_total_fields" => {
            lower_builtin(ctx, args, Builtin::GetTotalStructFields, node.span)?
        }
        "error" => lower_builtin(ctx, args, Builtin::Error, node.span)?,
        "add" => lower_builtin(ctx, args, Builtin::Add, node.span)?,
        "eq" => lower_builtin(ctx, args, Builtin::Eq, node.span)?,
        "mem__malloc" => lower_builtin(ctx, args, Builtin::Malloc, node.span)?,
        "mem__write" => lower_builtin(ctx, args, Builtin::MemWrite, node.span)?,
        "mem__read" => lower_builtin(ctx, args, Builtin::MemRead, node.span)?,
        "io__input_size" => lower_builtin(ctx, args, Builtin::InputSize, node.span)?,
        "io__input_copy" => lower_builtin(ctx, args, Builtin::InputCopy, node.span)?,
        "io__return_exit" => lower_builtin(ctx, args, Builtin::ReturnExit, node.span)?,
        _ => lower_apply(ctx, node.span, list)?,
    };
    Ok(e)
}

fn lower_builtin(
    ctx: &mut LoweringCtx,
    args: &[SNode],
    builtin: Builtin,
    span: Span<usize>,
) -> Result<Expr, ParseError> {
    if args.len() != builtin.arg_count() {
        return Err(ParseError {
            message: format!(
                "Invalid argument count for builtin {:?}, expected: {}, got: {}",
                builtin,
                builtin.arg_count(),
                args.len()
            ),
            span,
        });
    }

    let arguments = args
        .iter()
        .map(|arg| snode_to_expr(ctx, arg))
        .collect::<Result<_, _>>()?;

    Ok(Expr {
        span,
        kind: ExprKind::BuiltinCall(Box::new(BuiltinCall { builtin, arguments })),
    })
}

fn lower_let_bind(ctx: &mut LoweringCtx, node: &SNode) -> Result<LetBind, ParseError> {
    let let_list = expect_list(node)?;
    let span = node.span;

    // Check if first element is "comptime" keyword
    let (is_comptime, remaining) = match let_list {
        [first, rest @ ..] if is_comptime_keyword(first) => (true, rest),
        _ => (false, let_list),
    };

    match remaining {
        [name_node, type_node, value_node] => Ok(LetBind {
            span,
            is_comptime,
            bind_local: expect_name(name_node)?,
            bind_type_expr: snode_to_expr(ctx, type_node)?,
            assigned: snode_to_expr(ctx, value_node)?,
        }),
        _ => Err(ParseError {
            message: format!(
                "let binding must have 3-4 nodes (comptime? name type value), got {}",
                remaining.len()
            ),
            span,
        }),
    }
}

pub fn lower_sexpr_to_ast(main: &SNode) -> Result<Ast, ParseError> {
    let mut ctx = LoweringCtx::new();
    let runtime_main = snode_to_expr(&mut ctx, main)?;
    Ok(Ast { runtime_main })
}

#[cfg(test)]
mod tests {
    use super::*;

    fn parse_and_lower(source: &str) -> Result<Ast, ParseError> {
        let mut parser = Parser::new(source);
        let node = parser.parse_list()?.expect("expected at least one s-expr");
        lower_sexpr_to_ast(&node)
    }

    #[test]
    fn test_lower_constants() {
        let ast = parse_and_lower("(42)").unwrap();
        assert!(matches!(&ast.runtime_main.kind, ExprKind::Value(v) if matches!(&**v, Value::Num(42))));

        let ast = parse_and_lower("(true)").unwrap();
        assert!(matches!(&ast.runtime_main.kind, ExprKind::Value(v) if matches!(&**v, Value::Bool(true))));

        let ast = parse_and_lower("(())").unwrap();
        assert!(matches!(&ast.runtime_main.kind, ExprKind::Value(v) if matches!(&**v, Value::Void)));
    }

    #[test]
    fn test_lower_var() {
        let ast = parse_and_lower("(foo)").unwrap();
        assert!(matches!(&ast.runtime_main.kind, ExprKind::Var(name) if &**name == "foo"));
    }

    #[test]
    fn test_lower_if() {
        let ast = parse_and_lower("(if true 1 2)").unwrap();
        assert!(matches!(ast.runtime_main.kind, ExprKind::IfThenElse(_)));
    }

    #[test]
    fn test_lower_block() {
        // Block is now desugared to nested FuncApp/FuncDef
        // (block (x i32 10) (y i32 20) x) => ((λ x:i32. ((λ y:i32. x) 20)) 10)
        let ast = parse_and_lower("(block (x i32 10) (y i32 20) x)").unwrap();
        // Outer is FuncApp applying 10 to (λ x. ...)
        let ExprKind::FuncApp(outer_app) = &ast.runtime_main.kind else {
            panic!("Expected outer FuncApp");
        };
        // The func_expr should be a FuncDef (λ x. ...)
        let ExprKind::FuncDef(outer_func) = &outer_app.func_expr.kind else {
            panic!("Expected outer FuncDef");
        };
        assert_eq!(&*outer_func.func_bind.name, "x");
        // The body of outer_func should be another FuncApp
        let ExprKind::FuncApp(inner_app) = &outer_func.body.kind else {
            panic!("Expected inner FuncApp");
        };
        // Which applies to (λ y. ...)
        let ExprKind::FuncDef(inner_func) = &inner_app.func_expr.kind else {
            panic!("Expected inner FuncDef");
        };
        assert_eq!(&*inner_func.func_bind.name, "y");
    }

    #[test]
    fn test_lower_apply() {
        let ast = parse_and_lower("(apply f x y)").unwrap();
        // Should be FuncApp(FuncApp(f, x), y)
        let ExprKind::FuncApp(outer) = &ast.runtime_main.kind else {
            panic!("Expected FuncApp");
        };
        assert!(matches!(outer.func_expr.kind, ExprKind::FuncApp(_)));
    }

    #[test]
    fn test_lower_func() {
        let ast = parse_and_lower("(funcdef x word x)").unwrap();
        assert!(matches!(ast.runtime_main.kind, ExprKind::FuncDef(_)));
    }

    #[test]
    fn test_lower_attr() {
        let ast = parse_and_lower("(attr foo bar my_struct)").unwrap();
        // Should be MemberAccess(MemberAccess(my_struct, foo), bar)
        let ExprKind::MemberAccess(outer) = &ast.runtime_main.kind else {
            panic!("Expected MemberAccess");
        };
        assert_eq!(&*outer.member.name, "bar");
        let ExprKind::MemberAccess(inner) = &outer.r#struct.kind else {
            panic!("Expected inner MemberAccess");
        };
        assert_eq!(&*inner.member.name, "foo");
    }

    #[test]
    fn test_lower_struct() {
        let ast = parse_and_lower("(struct_def () (x word) (y bool))").unwrap();
        let ExprKind::StructDef(s) = &ast.runtime_main.kind else {
            panic!("Expected StructDef");
        };
        assert_eq!(s.fields.len(), 2);
        assert_eq!(s.def_uuid, 0);
    }

    #[test]
    fn test_lower_struct_init() {
        let ast = parse_and_lower("(struct_init Point (x 10) (y 20))").unwrap();
        let ExprKind::StructInit(s) = &ast.runtime_main.kind else {
            panic!("Expected StructInit");
        };
        assert_eq!(s.fields.len(), 2);
        assert_eq!(&*s.fields[0].name.name, "x");
        assert_eq!(&*s.fields[1].name.name, "y");
    }

    #[test]
    fn test_lower_struct_init_empty() {
        let ast = parse_and_lower("(struct_init EmptyStruct)").unwrap();
        let ExprKind::StructInit(s) = &ast.runtime_main.kind else {
            panic!("Expected StructInit");
        };
        assert_eq!(s.fields.len(), 0);
    }

    #[test]
    fn test_keyword_without_args_errors() {
        // Keywords without required arguments should error, not become Var
        assert!(parse_and_lower("(block)").is_err());
        assert!(parse_and_lower("(funcdef)").is_err());
        assert!(parse_and_lower("(if)").is_err());
        assert!(parse_and_lower("(attr)").is_err());
        assert!(parse_and_lower("(struct_def)").is_err());

        // Non-keywords should still work as Var (note: "apply" is not a keyword)
        let ast = parse_and_lower("(foo)").unwrap();
        assert!(matches!(&ast.runtime_main.kind, ExprKind::Var(name) if &**name == "foo"));
        let ast = parse_and_lower("(apply)").unwrap();
        assert!(matches!(&ast.runtime_main.kind, ExprKind::Var(name) if &**name == "apply"));
    }
}
