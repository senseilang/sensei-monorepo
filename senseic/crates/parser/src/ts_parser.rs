use allocator_api2::vec::Vec;
use bumpalo::Bump;
use smallvec::SmallVec;
use tree_sitter::{Node, Parser, TreeCursor};

use crate::{StringInterner, ast::*};

struct Cursor<'tree>(TreeCursor<'tree>);

impl<'tree> std::ops::Deref for Cursor<'tree> {
    type Target = TreeCursor<'tree>;
    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<'tree> std::ops::DerefMut for Cursor<'tree> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}

impl<'tree> Cursor<'tree> {
    fn skip_unnamed(&mut self, expected_kind: &'static str) {
        let skipping = self.next();
        debug_assert!(
            !skipping.is_named(),
            "tried to skip named node '{}' when expecting '{}'",
            skipping.kind(),
            expected_kind
        );
        debug_assert_eq!(skipping.kind(), expected_kind, "skipping different");
    }

    fn next(&mut self) -> Node<'tree> {
        let node = self.node();
        self.0.goto_next_sibling();
        node
    }

    fn next_expect(&mut self, expected_kind: &'static str) -> Node<'tree> {
        let node = self.next();
        debug_assert_eq!(node.kind(), expected_kind, "got != expected");
        node
    }

    fn enter_current_node<F, O>(&mut self, f: F) -> O
    where
        F: FnOnce(&mut Self) -> O,
    {
        let entered = self.0.goto_first_child();
        let output = f(self);
        if entered {
            self.0.goto_parent();
        }
        output
    }

    fn with_next<F, O>(&mut self, f: F) -> O
    where
        F: FnOnce(&mut Self, Node<'tree>) -> O,
    {
        let node = self.node();
        let output = f(self, node);
        self.goto_next_sibling();
        output
    }
}

#[derive(Debug, Clone)]
pub struct ParseError {
    kind: ParseErrorKind,
    span: SourceSpan,
}

#[derive(Debug, Clone)]
pub enum ParseErrorKind {
    InvalidSyntax,
    Missing(Box<str>),
}

fn check_ts_tree<'tree, E>(emit_error: &mut E, cursor: &mut Cursor<'tree>, node: Node<'tree>)
where
    E: FnMut(ParseError),
{
    let range = node.byte_range();
    if node.is_error() {
        emit_error(ParseError {
            kind: ParseErrorKind::InvalidSyntax,
            span: SourceSpan::new(range.start as u32, range.end as u32),
        });
    }
    if node.is_missing() {
        emit_error(ParseError {
            kind: ParseErrorKind::Missing(node.kind().into()),
            span: SourceSpan::new(range.start as u32, range.end as u32),
        });
    }

    let children = node.child_count();
    cursor.enter_current_node(|cursor| {
        for _ in 0..children {
            cursor.with_next(|cursor, node| check_ts_tree(emit_error, cursor, node))
        }
    });
}

pub fn parse_via_tree_sitter<'src, 'ast>(
    source: &'src str,
    arena: &'ast Bump,
) -> Result<(Ast<'ast>, StringInterner), Vec<ParseError, &'ast Bump>> {
    let mut interner = StringInterner::with_capacity_and_hasher(256, Default::default());
    let mut parser = Parser::new();

    let language = tree_sitter_sensei::LANGUAGE.into();
    parser.set_language(&language).expect("Error loading sensei tree-sitter language");

    let tree = parser.parse(source, None).expect("tree-sitter parsing failed");
    let root = tree.root_node();
    println!("root.to_sexp(): {}", root.to_sexp());
    let mut cursor = Cursor(root.walk());

    let mut errors = Vec::with_capacity_in(8, arena);
    let mut emit_error = |err| {
        errors.push(err);
    };

    check_ts_tree(&mut emit_error, &mut cursor, root);

    if !errors.is_empty() {
        return Err(errors);
    }

    let ast = convert_root_to_ast(root, source, arena, &mut cursor, &mut interner);

    Ok((ast, interner))
}

fn convert_root_to_ast<'tree, 'src, 'ast>(
    root: Node<'tree>,
    source: &'src str,
    arena: &'ast Bump,
    cursor: &mut Cursor<'tree>,
    interner: &mut StringInterner,
) -> Ast<'ast> {
    let mut declarations = Vec::with_capacity_in(32, arena);

    cursor.enter_current_node(|cursor| {
        for _ in 0..root.child_count() {
            cursor.with_next(|cursor, declaration| {
                declarations.extend(convert_decl(declaration, cursor, source, arena, interner));
            })
        }
    });

    Ast { declarations }
}

fn convert_decl<'tree, 'src, 'ast>(
    declaration: Node<'tree>,
    cursor: &mut Cursor<'tree>,
    source: &'src str,
    arena: &'ast Bump,
    interner: &mut StringInterner,
) -> Option<Declaration<'ast>> {
    if !declaration.is_named() {
        return None;
    }

    Some(match declaration.kind() {
        "const_def" => cursor.enter_current_node(|cursor| {
            let is_public = cursor.node().kind() == "export";
            if is_public {
                cursor.goto_next_sibling();
            }
            let const_def = cursor
                .enter_current_node(|cursor| convert_const_item(cursor, source, arena, interner));
            cursor.next_expect("const_item");
            cursor.skip_unnamed(";");
            if is_public {
                Declaration::PublicConstDef(const_def)
            } else {
                Declaration::ConstDef(const_def)
            }
        }),
        "run" => Declaration::Run(cursor.enter_current_node(|cursor| {
            cursor.skip_unnamed("run");
            let block = cursor.next_expect("block");
            convert_block(block, cursor, source, arena, interner)
        })),
        "init" => Declaration::Init(cursor.enter_current_node(|cursor| {
            cursor.skip_unnamed("init");
            let block = cursor.next_expect("block");
            convert_block(block, cursor, source, arena, interner)
        })),
        "import" => Declaration::Import(convert_import(cursor, source, arena, interner)),
        kind => panic!("unexpected declaration kind: {:?}", kind),
    })
}

fn convert_import<'tree, 'src, 'ast>(
    cursor: &mut Cursor<'tree>,
    source: &'src str,
    arena: &'ast Bump,
    interner: &mut StringInterner,
) -> Import<'ast> {
    let import = cursor.node();
    let kind = import.child_by_field_name("kind").expect("missing 'kind' field");

    let kind = match kind.kind() {
        "import_all" => match kind.child_by_field_name("as") {
            Some(ident) => ImportKind::As(interner.intern(&source[ident.byte_range()])),
            None => ImportKind::All,
        },
        "import_select" => {
            let mut selected = SmallVec::<[IStr; 32]>::new();
            let mut fresh_cursor = kind.walk();
            for selection in kind.children_by_field_name("selection", &mut fresh_cursor) {
                selected.push(interner.intern(&source[selection.byte_range()]));
            }
            ImportKind::Selction(arena.alloc(selected))
        }
        kind => unreachable!("unexpected import kind {kind:?}"),
    };

    let path = import.child_by_field_name("path").expect("missing 'path' field");
    let start = path.start_byte() + 1;
    let end = path.end_byte() - 1;
    let path = &source[start..end];

    Import { kind, path: arena.alloc_str(path) }
}

fn convert_const_item<'tree, 'src, 'ast>(
    cursor: &mut Cursor<'tree>,
    source: &'src str,
    arena: &'ast Bump,
    interner: &mut StringInterner,
) -> ConstDef<'ast> {
    cursor.enter_current_node(|cursor| {
        cursor.skip_unnamed("const");

        let ident_node = cursor.next_expect("identifier");
        let ident_text = &source[ident_node.byte_range()];
        let ident = interner.intern(ident_text);

        let r#type = (cursor.node().kind() == ":").then(|| {
            cursor.skip_unnamed(":");
            cursor.with_next(|cursor, type_node| {
                convert_type_expr(type_node, cursor, source, arena, interner)
            })
        });

        cursor.skip_unnamed("=");
        let expr = cursor.with_next(|cursor, expr_node| {
            convert_expr(expr_node, cursor, source, arena, interner)
        });

        ConstDef { ident, r#type, expr }
    })
}

fn convert_block<'tree, 'src, 'ast>(
    block: Node<'tree>,
    cursor: &mut Cursor<'tree>,
    source: &'src str,
    arena: &'ast Bump,
    interner: &mut StringInterner,
) -> Block<'ast> {
    cursor.enter_current_node(|cursor| {
        let mut last_expr = None;
        let mut statements = SmallVec::<[Statement<'ast>; 16]>::new();

        for _ in 0..block.child_count() {
            cursor.with_next(|cursor, child| {
                let field = cursor.field_name();
                if !child.is_named() {
                    return;
                }

                match field {
                    Some("stmts") => {
                        statements.push(convert_stmt(child, cursor, source, arena, interner))
                    }
                    Some("last_expr") => {
                        last_expr =
                            Some(arena.alloc(convert_expr(child, cursor, source, arena, interner)));
                    }
                    _ => panic!("Node {:?} with unexpected field {:?}", child.kind(), field),
                }
            })
        }
        Block { statements: arena.alloc(statements), last_expr }
    })
}

fn convert_stmt<'tree, 'src, 'ast>(
    stmt: Node<'tree>,
    cursor: &mut Cursor<'tree>,
    source: &'src str,
    arena: &'ast Bump,
    interner: &mut StringInterner,
) -> Statement<'ast> {
    match stmt.kind() {
        "let" => Statement::Let(arena.alloc(convert_let(cursor, source, arena, interner))),
        "return" => {
            let return_expr = convert_return(cursor, source, arena, interner);
            Statement::Return(return_expr)
        }
        "assign" => {
            let assign_stmt = convert_assign(cursor, source, arena, interner);
            Statement::Assign(arena.alloc(assign_stmt))
        }
        "const_item" => {
            let const_def = convert_const_item(cursor, source, arena, interner);
            Statement::ConstDef(arena.alloc(const_def))
        }
        "block" => {
            let block = convert_block(stmt, cursor, source, arena, interner);
            Statement::Block(block)
        }
        "identifier" | "bool_literal" | "hex_literal" | "bin_literal" | "dec_literal"
        | "fn_def" | "struct_def" | "name_path" | "binary_expr" | "fn_call" | "member"
        | "struct_lit" => {
            let expr = convert_expr(stmt, cursor, source, arena, interner);
            Statement::Expr(expr)
        }
        kind => panic!("Unknown stmt node {:?}", kind),
    }
}

fn convert_return<'tree, 'src, 'ast>(
    cursor: &mut Cursor<'tree>,
    source: &'src str,
    arena: &'ast Bump,
    interner: &mut StringInterner,
) -> Expr<'ast> {
    cursor.enter_current_node(|cursor| {
        cursor.skip_unnamed("return");
        cursor
            .with_next(|cursor, expr_node| convert_expr(expr_node, cursor, source, arena, interner))
    })
}

fn convert_assign<'tree, 'src, 'ast>(
    cursor: &mut Cursor<'tree>,
    source: &'src str,
    arena: &'ast Bump,
    interner: &mut StringInterner,
) -> AssignStmt<'ast> {
    cursor.enter_current_node(|cursor| {
        let target = cursor.with_next(|cursor, target_node| {
            debug_assert_eq!(target_node.kind(), "name_path");
            convert_name_path(target_node, cursor, source, arena, interner)
        });

        cursor.skip_unnamed("=");
        let value = cursor.with_next(|cursor, value_node| {
            convert_expr(value_node, cursor, source, arena, interner)
        });

        AssignStmt { target, op: AssignOp::Assign, value }
    })
}

fn convert_let<'tree, 'src, 'ast>(
    cursor: &mut Cursor<'tree>,
    source: &'src str,
    arena: &'ast Bump,
    interner: &mut StringInterner,
) -> LetStmt<'ast> {
    cursor.enter_current_node(|cursor| {
        cursor.skip_unnamed("let");

        let mutable = if !cursor.node().is_named() {
            cursor.skip_unnamed("mut");
            true
        } else {
            false
        };

        let ident_node = cursor.next_expect("identifier");
        let ident_text = &source[ident_node.byte_range()];
        let ident = interner.intern(ident_text);

        let r#type = (cursor.node().kind() == ":").then(|| {
            cursor.skip_unnamed(":");
            cursor.with_next(|cursor, type_node| {
                convert_type_expr(type_node, cursor, source, arena, interner)
            })
        });

        cursor.skip_unnamed("=");
        let value = cursor.with_next(|cursor, value_node| {
            convert_expr(value_node, cursor, source, arena, interner)
        });

        LetStmt { mutable, ident, r#type, value }
    })
}

fn convert_expr<'tree, 'src, 'ast>(
    expr: Node<'tree>,
    cursor: &mut Cursor<'tree>,
    source: &'src str,
    arena: &'ast Bump,
    interner: &mut StringInterner,
) -> Expr<'ast> {
    use neosen_data::bigint::FrozenBigUint;
    match expr.kind() {
        "identifier" => {
            let ident_text = &source[expr.byte_range()];
            let ident = interner.intern(ident_text);
            Expr::Ident(ident)
        }
        "bool_literal" => {
            let value = cursor.enter_current_node(|cursor| match cursor.node().kind() {
                "true" => true,
                "false" => false,
                kind => panic!("Invalid bool literal: {:?}", kind),
            });
            Expr::BoolLiteral(value)
        }
        "hex_literal" => {
            let digits = &source[expr.byte_range()];
            let positive = !digits.starts_with('-');
            let digits = digits.strip_prefix('-').unwrap_or(digits);
            debug_assert!(digits.starts_with("0x"));
            let num = FrozenBigUint::from_radix16_in(&digits[2..], arena);
            Expr::IntLiteral(IntLiteral { positive, num })
        }
        "bin_literal" => {
            let digits = &source[expr.byte_range()];
            let positive = !digits.starts_with('-');
            let digits = digits.strip_prefix('-').unwrap_or(digits);
            debug_assert!(digits.starts_with("0b"));
            let num = FrozenBigUint::from_radix2_in(&digits[2..], arena);
            Expr::IntLiteral(IntLiteral { positive, num })
        }
        "dec_literal" => {
            let digits = &source[expr.byte_range()];
            let positive = !digits.starts_with('-');
            let digits = digits.strip_prefix('-').unwrap_or(digits);
            let num = neosen_data::bigint::FrozenBigUint::from_radix10_in(digits, arena);
            Expr::IntLiteral(IntLiteral { positive, num })
        }
        "block" => {
            let block = convert_block(expr, cursor, source, arena, interner);
            Expr::Block(block)
        }
        "fn_def" | "struct_def" | "type_def" => {
            let type_expr = convert_type_expr(expr, cursor, source, arena, interner);
            Expr::TypeExpr(type_expr)
        }
        "name_path" => {
            let name_path = convert_name_path(expr, cursor, source, arena, interner);
            Expr::TypeExpr(TypeExpr::NamePath(name_path))
        }
        "binary_expr" => {
            let binary_expr = convert_binary_expr(cursor, source, arena, interner);
            Expr::Binary(binary_expr)
        }
        "fn_call" => {
            let fn_call = convert_fn_call(expr, cursor, source, arena, interner);
            Expr::FnCall(fn_call)
        }
        "member" => {
            let member = convert_member(cursor, source, arena, interner);
            Expr::Member(member)
        }
        "struct_lit" => {
            let struct_lit = convert_struct_lit(cursor, source, arena, interner);
            Expr::StructLiteral(arena.alloc(struct_lit))
        }
        "paren_expr" => cursor.enter_current_node(|cursor| {
            cursor.skip_unnamed("(");
            let expr = cursor
                .with_next(|cursor, expr| convert_expr(expr, cursor, source, arena, interner));
            cursor.skip_unnamed(")");
            expr
        }),
        kind => panic!("Unimplemented expression kind: {:?}", kind),
    }
}

fn convert_binary_expr<'tree, 'src, 'ast>(
    cursor: &mut Cursor<'tree>,
    source: &'src str,
    arena: &'ast Bump,
    interner: &mut StringInterner,
) -> BinaryExpr<'ast> {
    cursor.enter_current_node(|cursor| {
        let lhs = cursor.with_next(|cursor, child| {
            debug_assert_eq!(cursor.field_name(), Some("lhs"));
            arena.alloc(convert_expr(child, cursor, source, arena, interner))
        });
        let op = match cursor.next().kind() {
            "+%" => BinaryOp::AddWrap,
            "+" => BinaryOp::AddChecked,
            "-%" => BinaryOp::SubWrap,
            "-" => BinaryOp::SubChecked,
            "*%" => BinaryOp::MulWrap,
            "*" => BinaryOp::MulChecked,
            "/+" => BinaryOp::DivToPos,
            "/-" => BinaryOp::DivToNeg,
            "/$" => BinaryOp::DivToZero,
            "/^" => BinaryOp::DivFromZero,
            "<" => BinaryOp::LessThan,
            "<=" => BinaryOp::LessThanEquals,
            ">" => BinaryOp::GreaterThan,
            ">=" => BinaryOp::GreaterThanEquals,
            "==" => BinaryOp::EqualEqual,
            op => panic!("Unknown binary operator: {:?}", op),
        };
        let rhs = cursor.with_next(|cursor, child| {
            debug_assert_eq!(cursor.field_name(), Some("rhs"));
            arena.alloc(convert_expr(child, cursor, source, arena, interner))
        });

        BinaryExpr { lhs, op, rhs }
    })
}

fn convert_fn_call<'tree, 'src, 'ast>(
    fn_call: Node<'tree>,
    cursor: &mut Cursor<'tree>,
    source: &'src str,
    arena: &'ast Bump,
    interner: &mut StringInterner,
) -> FnCall<'ast> {
    let child_count = fn_call.child_count();
    cursor.enter_current_node(|cursor| {
        let fn_expr = cursor.with_next(|cursor, fn_expr| {
            debug_assert_eq!(cursor.field_name(), Some("fn"));
            arena.alloc(convert_expr(fn_expr, cursor, source, arena, interner))
        });
        let mut param_exprs = SmallVec::<[Expr<'ast>; 8]>::new();

        for _ in 0..child_count - 1 {
            cursor.with_next(|cursor, child| {
                if !child.is_named() {
                    return;
                }
                param_exprs.push(convert_expr(child, cursor, source, arena, interner));
            });
        }

        FnCall { fn_expr, param_exprs: arena.alloc(param_exprs) }
    })
}

fn convert_member<'tree, 'src, 'ast>(
    cursor: &mut Cursor<'tree>,
    source: &'src str,
    arena: &'ast Bump,
    interner: &mut StringInterner,
) -> Member<'ast> {
    cursor.enter_current_node(|cursor| {
        let expr_node = cursor.node();
        let expr = arena.alloc(convert_expr(expr_node, cursor, source, arena, interner));
        cursor.goto_next_sibling();

        cursor.skip_unnamed(".");
        let ident_node = cursor.next_expect("identifier");
        let ident_text = &source[ident_node.byte_range()];
        let ident = interner.intern(ident_text);

        Member { expr, ident }
    })
}

fn convert_type_expr<'tree, 'src, 'ast>(
    type_expr: Node<'tree>,
    cursor: &mut Cursor<'tree>,
    source: &'src str,
    arena: &'ast Bump,
    interner: &mut StringInterner,
) -> TypeExpr<'ast> {
    let kind = type_expr.kind();

    match kind {
        "type_expr" | "type_def" => cursor.enter_current_node(|cursor| {
            cursor.with_next(|cursor, child| {
                convert_type_expr(child, cursor, source, arena, interner)
            })
        }),
        "name_path" | "identifier" => {
            let name_path = convert_name_path(type_expr, cursor, source, arena, interner);
            TypeExpr::NamePath(name_path)
        }
        "fn_def" => {
            let fn_def = convert_fn_def(cursor, source, arena, interner);
            TypeExpr::FnDef(arena.alloc(fn_def))
        }
        "struct_def" => {
            let struct_def = convert_struct_def(cursor, source, arena, interner);
            TypeExpr::StructDef(struct_def)
        }
        kind => panic!("Unimplemented type expression kind: {:?}", kind),
    }
}

fn convert_name_path<'tree, 'src, 'ast>(
    name_path: Node<'tree>,
    cursor: &mut Cursor<'tree>,
    source: &'src str,
    arena: &'ast Bump,
    interner: &mut StringInterner,
) -> NamePath<'ast> {
    if name_path.kind() == "identifier" {
        let ident_text = &source[name_path.byte_range()];
        let ident = interner.intern(ident_text);
        return NamePath(arena.alloc([ident]));
    }

    cursor.enter_current_node(|cursor| {
        let mut segments = SmallVec::<[IStr; 4]>::new();
        loop {
            let ident_node = cursor.next_expect("identifier");
            let ident = &source[ident_node.byte_range()];
            segments.push(interner.intern(ident));
            if !cursor.goto_next_sibling() {
                break;
            }
        }

        NamePath(arena.alloc(segments))
    })
}

fn convert_fn_def<'tree, 'src, 'ast>(
    cursor: &mut Cursor<'tree>,
    source: &'src str,
    arena: &'ast Bump,
    interner: &mut StringInterner,
) -> FnDef<'ast> {
    cursor.enter_current_node(|cursor| {
        cursor.skip_unnamed("fn");

        let mut params = SmallVec::<[ParamDef<'ast>; 8]>::new();
        cursor.enter_current_node(|cursor| {
            while cursor.goto_next_sibling() {
                if !cursor.node().is_named() {
                    continue;
                }
                params.push(convert_typed_item_as_param(cursor, source, arena, interner));
            }
        });
        cursor.next_expect("fn_def_params");

        let result = cursor.with_next(|cursor, result_node| {
            convert_type_expr(result_node, cursor, source, arena, interner)
        });

        let body = cursor.with_next(|cursor, block_node| {
            debug_assert_eq!(block_node.kind(), "block");
            convert_block(block_node, cursor, source, arena, interner)
        });

        FnDef { params: arena.alloc(params), result, body }
    })
}

fn convert_struct_def<'tree, 'src, 'ast>(
    cursor: &mut Cursor<'tree>,
    source: &'src str,
    arena: &'ast Bump,
    interner: &mut StringInterner,
) -> StructDef<'ast> {
    cursor.enter_current_node(|cursor| {
        let mut fields = SmallVec::<[FieldDef<'ast>; 8]>::new();

        cursor.skip_unnamed("struct");
        cursor.skip_unnamed("{");

        while cursor.node().kind() != "}" {
            fields.push(convert_typed_item_as_field(cursor, source, arena, interner));
            cursor.next_expect("typed_item_def");
            if cursor.node().kind() == "," {
                cursor.goto_next_sibling();
            }
        }

        StructDef { fields: arena.alloc(fields) }
    })
}

fn convert_typed_item_as_param<'tree, 'src, 'ast>(
    cursor: &mut Cursor<'tree>,
    source: &'src str,
    arena: &'ast Bump,
    interner: &mut StringInterner,
) -> ParamDef<'ast> {
    debug_assert_eq!(cursor.node().kind(), "typed_item_def");
    cursor.enter_current_node(|cursor| {
        let name_node = cursor.next_expect("identifier");
        let name_text = &source[name_node.byte_range()];
        let name = interner.intern(name_text);

        cursor.skip_unnamed(":");
        let r#type = cursor.with_next(|cursor, type_node| {
            convert_type_expr(type_node, cursor, source, arena, interner)
        });

        ParamDef { name, r#type }
    })
}

fn convert_typed_item_as_field<'tree, 'src, 'ast>(
    cursor: &mut Cursor<'tree>,
    source: &'src str,
    arena: &'ast Bump,
    interner: &mut StringInterner,
) -> FieldDef<'ast> {
    cursor.enter_current_node(|cursor| {
        let name_node = cursor.next_expect("identifier");
        let name_text = &source[name_node.byte_range()];
        let name = interner.intern(name_text);

        cursor.skip_unnamed(":");
        let r#type = cursor.with_next(|cursor, type_node| {
            convert_type_expr(type_node, cursor, source, arena, interner)
        });

        FieldDef { name, r#type }
    })
}

fn convert_struct_lit<'tree, 'src, 'ast>(
    cursor: &mut Cursor<'tree>,
    source: &'src str,
    arena: &'ast Bump,
    interner: &mut StringInterner,
) -> StructLiteral<'ast> {
    cursor.enter_current_node(|cursor| {
        let type_path = cursor.with_next(|cursor, name_path_node| {
            debug_assert_eq!(name_path_node.kind(), "name_path");
            convert_name_path(name_path_node, cursor, source, arena, interner)
        });

        let mut fields = SmallVec::<[FieldInit<'ast>; 8]>::new();
        cursor.enter_current_node(|cursor| {
            while cursor.goto_next_sibling() {
                let node = cursor.node();
                if !cursor.node().is_named() {
                    continue;
                }
                debug_assert_eq!(node.kind(), "struct_lit_field");
                fields.push(cursor.enter_current_node(|cursor| {
                    let name_node = cursor.next_expect("identifier");
                    let name_text = &source[name_node.byte_range()];
                    let name = interner.intern(name_text);

                    cursor.skip_unnamed(":");
                    let value = cursor.with_next(|cursor, expr_node| {
                        convert_expr(expr_node, cursor, source, arena, interner)
                    });

                    FieldInit { name, value }
                }));
            }
        });
        cursor.next_expect("struct_lit_fields");

        StructLiteral { type_path, fields: arena.alloc(fields) }
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::display::AstDisplay;

    // Helper to parse and return AST display string (leaks memory for test simplicity)
    fn parse_to_string(source: &str) -> String {
        let arena = Box::leak(Box::new(Bump::new()));
        let (ast, interner) = parse_via_tree_sitter(source, arena).expect("parser had errors");
        // Leak the AST and interner to get 'static references
        let ast_ref: &'static Ast<'static> =
            Box::leak(Box::new(unsafe { std::mem::transmute(ast) }));
        let interner_ref: &'static StringInterner =
            Box::leak(Box::new(unsafe { std::mem::transmute(interner) }));
        format!("{}", AstDisplay::new(ast_ref, interner_ref))
    }

    #[test]
    fn test_empty() {
        insta::assert_snapshot!(parse_to_string(""), @"(ast)");
    }

    #[test]
    fn test_simple_let() {
        insta::assert_snapshot!(parse_to_string("init { let x = 42; }"), @r#"
        (ast
          (init
            (block
              (let "x"
                (value
                  (int 0x2a)
                )
              )
            )
          )
        )
        "#);
    }

    #[test]
    fn test_let_mut() {
        insta::assert_snapshot!(parse_to_string("init { let mut y = 100; }"), @r#"
        (ast
          (init
            (block
              (let "y" mut
                (value
                  (int 0x64)
                )
              )
            )
          )
        )
        "#);
    }

    #[test]
    fn test_const_def() {
        insta::assert_snapshot!(parse_to_string("const FOO = 123;"), @r#"
        (ast
          (private-const-def "FOO"
            (value
              (int 0x7b)
            )
          )
        )
        "#);
    }

    #[test]
    fn test_const_with_type() {
        insta::assert_snapshot!(parse_to_string("const BAR: u256 = 456;"), @r#"
        (ast
          (private-const-def "BAR"
            (type
              (name-path "u256")
            )
            (value
              (int 0x1c8)
            )
          )
        )
        "#);
    }

    #[test]
    fn test_binary_expr() {
        insta::assert_snapshot!(parse_to_string("init { let x = 1 + 2; }"), @r#"
        (ast
          (init
            (block
              (let "x"
                (value
                  (binary +
                    (lhs
                      (int 0x1)
                    )
                    (rhs
                      (int 0x2)
                    )
                  )
                )
              )
            )
          )
        )
        "#);
    }

    #[test]
    fn test_function_call() {
        insta::assert_snapshot!(parse_to_string("init { foo(1, 2, 3); }"), @r#"
        (ast
          (init
            (block
              (expr
                (fn-call
                  (fn
                    (ident "foo")
                  )
                  (args
                    (int 0x1)
                    (int 0x2)
                    (int 0x3)
                  )
                )
              )
            )
          )
        )
        "#);
    }

    #[test]
    fn test_member_access() {
        insta::assert_snapshot!(parse_to_string("init { let x = obj.field; }"), @r#"
        (ast
          (init
            (block
              (let "x"
                (value
                  (member "field"
                    (ident "obj")
                  )
                )
              )
            )
          )
        )
        "#);
    }

    #[test]
    fn test_struct_def() {
        insta::assert_snapshot!(parse_to_string("const Point = struct { x: u256, y: u256 };"), @r#"
        (ast
          (private-const-def "Point"
            (value
              (type-expr
                (struct-def
                  (fields
                    (field "x"
                      (name-path "u256")
                    )
                    (field "y"
                      (name-path "u256")
                    )
                  )
                )
              )
            )
          )
        )
        "#);
    }

    #[test]
    fn test_struct_def_trailing() {
        insta::assert_snapshot!(parse_to_string("const Point = struct { x: u256, y: u256, };"), @r#"
        (ast
          (private-const-def "Point"
            (value
              (type-expr
                (struct-def
                  (fields
                    (field "x"
                      (name-path "u256")
                    )
                    (field "y"
                      (name-path "u256")
                    )
                  )
                )
              )
            )
          )
        )
        "#);
    }

    #[test]
    fn test_fn_def() {
        insta::assert_snapshot!(parse_to_string("const add = fn (a: u256, b: u256) u256 { a + b };"), @r#"
        (ast
          (private-const-def "add"
            (value
              (type-expr
                (fn-def
                  (params
                    (param "a"
                      (name-path "u256")
                    )
                    (param "b"
                      (name-path "u256")
                    )
                  )
                  (result
                    (name-path "u256")
                  )
                  (body
                    (block
                      (trailing-expr
                        (binary +
                          (lhs
                            (ident "a")
                          )
                          (rhs
                            (ident "b")
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
        "#);
    }

    #[test]
    fn test_init_and_run() {
        insta::assert_snapshot!(parse_to_string("init { let x = 1; } run { let y = 2; }"), @r#"
        (ast
          (init
            (block
              (let "x"
                (value
                  (int 0x1)
                )
              )
            )
          )
          (run
            (block
              (let "y"
                (value
                  (int 0x2)
                )
              )
            )
          )
        )
        "#);
    }

    #[test]
    fn test_bool_literals() {
        insta::assert_snapshot!(parse_to_string("init { let t = true; let f = false; }"), @r#"
        (ast
          (init
            (block
              (let "t"
                (value
                  (bool true)
                )
              )
              (let "f"
                (value
                  (bool false)
                )
              )
            )
          )
        )
        "#);
    }

    #[test]
    fn test_hex_literal() {
        insta::assert_snapshot!(parse_to_string("const X = 0xDEADBEEF;"), @r#"
        (ast
          (private-const-def "X"
            (value
              (int 0xdeadbeef)
            )
          )
        )
        "#);
    }

    #[test]
    fn test_binary_literal() {
        insta::assert_snapshot!(parse_to_string("const B = 0b1010;"), @r#"
        (ast
          (private-const-def "B"
            (value
              (int 0xa)
            )
          )
        )
        "#);
    }

    #[test]
    fn test_fn_empty_params() {
        insta::assert_snapshot!(parse_to_string("const func = fn () void {};"), @r#"
            (ast
              (private-const-def "func"
                (value
                  (type-expr
                    (fn-def
                      (params)
                      (result
                        (name-path "void")
                      )
                      (body
                        (block)
                      )
                    )
                  )
                )
              )
            )"#);
    }

    #[test]
    fn test_fn_params_no_trailing() {
        insta::assert_snapshot!(parse_to_string("const func = fn (x: u256, y: u256) void {};"), @r#"
            (ast
              (private-const-def "func"
                (value
                  (type-expr
                    (fn-def
                      (params
                        (param "x"
                          (name-path "u256")
                        )
                        (param "y"
                          (name-path "u256")
                        )
                      )
                      (result
                        (name-path "void")
                      )
                      (body
                        (block)
                      )
                    )
                  )
                )
              )
            )"#);
    }

    #[test]
    fn test_fn_params_trailing_comma() {
        insta::assert_snapshot!(parse_to_string("const func = fn (y: u256, x: u256,) void {};"), @r#"
            (ast
              (private-const-def "func"
                (value
                  (type-expr
                    (fn-def
                      (params
                        (param "y"
                          (name-path "u256")
                        )
                        (param "x"
                          (name-path "u256")
                        )
                      )
                      (result
                        (name-path "void")
                      )
                      (body
                        (block)
                      )
                    )
                  )
                )
              )
            )"#);
    }

    #[test]
    fn test_fn_params_trailing_comma_standalone() {
        insta::assert_snapshot!(parse_to_string("const func = fn (y: u256,) void {};"), @r#"
            (ast
              (private-const-def "func"
                (value
                  (type-expr
                    (fn-def
                      (params
                        (param "y"
                          (name-path "u256")
                        )
                      )
                      (result
                        (name-path "void")
                      )
                      (body
                        (block)
                      )
                    )
                  )
                )
              )
            )"#);
    }

    #[test]
    fn test_return_statement() {
        insta::assert_snapshot!(parse_to_string("const func = fn () u256 { return 42; };"), @r#"
        (ast
          (private-const-def "func"
            (value
              (type-expr
                (fn-def
                  (params)
                  (result
                    (name-path "u256")
                  )
                  (body
                    (block
                      (return
                        (int 0x2a)
                      )
                    )
                  )
                )
              )
            )
          )
        )
        "#);
    }

    #[test]
    fn test_assign_statement() {
        insta::assert_snapshot!(parse_to_string("init { let mut x = 0; x = 10; }"), @r#"
        (ast
          (init
            (block
              (let "x" mut
                (value
                  (int 0x0)
                )
              )
              (assign =
                (target (name-path "x"))
                (value
                  (int 0xa)
                )
              )
            )
          )
        )
        "#);
    }

    #[test]
    fn test_nested_blocks() {
        insta::assert_snapshot!(parse_to_string("init { { let x = 1; } let y = 2; }"), @r#"
        (ast
          (init
            (block
              (block
                (let "x"
                  (value
                    (int 0x1)
                  )
                )
              )
              (let "y"
                (value
                  (int 0x2)
                )
              )
            )
          )
        )
        "#);
    }

    #[test]
    fn test_trailing_expression() {
        insta::assert_snapshot!(parse_to_string("const f = fn () u256 { 123 };"), @r#"
        (ast
          (private-const-def "f"
            (value
              (type-expr
                (fn-def
                  (params)
                  (result
                    (name-path "u256")
                  )
                  (body
                    (block
                      (trailing-expr
                        (int 0x7b)
                      )
                    )
                  )
                )
              )
            )
          )
        )
        "#);
    }

    #[test]
    fn test_complex_binary_ops() {
        insta::assert_snapshot!(parse_to_string("init { let a = 1 +% 2; let b = 3 *% 4; let c = 5 /+ 6; }"), @r#"
        (ast
          (init
            (block
              (let "a"
                (value
                  (binary +%
                    (lhs
                      (int 0x1)
                    )
                    (rhs
                      (int 0x2)
                    )
                  )
                )
              )
              (let "b"
                (value
                  (binary *%
                    (lhs
                      (int 0x3)
                    )
                    (rhs
                      (int 0x4)
                    )
                  )
                )
              )
              (let "c"
                (value
                  (binary /+
                    (lhs
                      (int 0x5)
                    )
                    (rhs
                      (int 0x6)
                    )
                  )
                )
              )
            )
          )
        )
        "#);
    }

    #[test]
    fn test_comparison_ops() {
        insta::assert_snapshot!(parse_to_string("init { let a = 1 < 2; let b = 3 >= (3 + 4); let c = 5 == 6; let z = (24); }"), @r#"
            (ast
              (init
                (block
                  (let "a"
                    (value
                      (binary <
                        (lhs
                          (int 0x1)
                        )
                        (rhs
                          (int 0x2)
                        )
                      )
                    )
                  )
                  (let "b"
                    (value
                      (binary >=
                        (lhs
                          (int 0x3)
                        )
                        (rhs
                          (binary +
                            (lhs
                              (int 0x3)
                            )
                            (rhs
                              (int 0x4)
                            )
                          )
                        )
                      )
                    )
                  )
                  (let "c"
                    (value
                      (binary ==
                        (lhs
                          (int 0x5)
                        )
                        (rhs
                          (int 0x6)
                        )
                      )
                    )
                  )
                  (let "z"
                    (value
                      (int 0x18)
                    )
                  )
                )
              )
            )"#);
    }

    #[test]
    fn test_name_path() {
        insta::assert_snapshot!(parse_to_string("const T: module.Type = 0;"), @r#"
        (ast
          (private-const-def "T"
            (type
              (name-path "module" "Type")
            )
            (value
              (int 0x0)
            )
          )
        )
        "#);
    }

    #[test]
    fn test_chained_member_access() {
        insta::assert_snapshot!(parse_to_string("init { let x = a.b.c; }"), @r#"
        (ast
          (init
            (block
              (let "x"
                (value
                  (member "c"
                    (member "b"
                      (ident "a")
                    )
                  )
                )
              )
            )
          )
        )
        "#);
    }

    #[test]
    fn test_struct_literal() {
        insta::assert_snapshot!(parse_to_string("init { let p = Point { x: 10, y: 20 }; }"), @r#"
        (ast
          (init
            (block
              (let "p"
                (value
                  (struct-literal
                    (type (name-path "Point"))
                    (fields
                      (field "x"
                        (int 0xa)
                      )
                      (field "y"
                        (int 0x14)
                      )
                    )
                  )
                )
              )
            )
          )
        )
        "#);
    }

    #[test]
    fn test_struct_literal_trailing() {
        insta::assert_snapshot!(parse_to_string("init { let p = Point { x: 10, y: 20, }; }"), @r#"
        (ast
          (init
            (block
              (let "p"
                (value
                  (struct-literal
                    (type (name-path "Point"))
                    (fields
                      (field "x"
                        (int 0xa)
                      )
                      (field "y"
                        (int 0x14)
                      )
                    )
                  )
                )
              )
            )
          )
        )
        "#);
    }

    #[test]
    fn test_negative_int() {
        insta::assert_snapshot!(parse_to_string("init { let x = -3; let y = -0x3af; }"), @r#"
            (ast
              (init
                (block
                  (let "x"
                    (value
                      (int -0x3)
                    )
                  )
                  (let "y"
                    (value
                      (int -0x3af)
                    )
                  )
                )
              )
            )
        "#);
    }
}
