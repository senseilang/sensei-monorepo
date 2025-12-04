use crate::ast::*;
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

fn sexpr_to_apply(children: &[SNode]) -> Result<ExprKind, ParseError> {
    children.split_first_chunk
    todo!()
}

fn snode_to_expr(node: &SNode) -> Result<Expr, ParseError> {
    let expr = |kind| Expr {
        span: node.span,
        kind,
    };
    let e = match node.kind {
        SNodeKind::Num(x) => expr(ExprKind::ConstInt(x)),
        SNodeKind::Bool(b) => expr(ExprKind::ConstBool(b)),
        SNodeKind::Name(name) => expr(ExprKind::Var(name.into())),
        SNodeKind::List(ref list) => match list.as_slice() {
            [] => expr(ExprKind::ConstVoid),
            [single] => snode_to_expr(single)?,
            [kind_name, tail @ ..] => match kind_name.kind {
                SNodeKind::Name("apply") => expr(sexpr_to_apply(tail)?),
                _ => {
                    return Err(ParseError {
                        message: format!("Expected 1/2/3 got: {:?}", kind_name),
                        span: kind_name.span,
                    });
                }
            },
        },
    };
    Ok(e)
}

pub fn lower_sexpr_to_ast(source: &str, exprs: &[SNode]) -> Result<Ast, ParseError> {
    let mut definitions = Vec::new();

    let (main, def_nodes) = exprs.split_last().ok_or_else(|| ParseError {
        message: format!("Need at least one node for the main function"),
        span: Span::new(source.len(), source.len()),
    })?;

    let runtime_main = snode_to_expr(main)?;

    Ok(Ast {
        definitions,
        runtime_main,
    })
}

// struct Definitions<'src> {
//     next_type_id: u32,
//     types: HashMap<&'src str, Type>,
//     functions: HashMap<&'src str, FunctionDef>,
// }
//
// impl<'src> Default for Definitions<'src> {
//     fn default() -> Self {
//         let mut types = HashMap::new();
//         types.insert("word", Type::Word);
//         types.insert("void", Type::Void);
//         types.insert("bool", Type::Bool);
//         Self {
//             next_type_id: 0,
//             types,
//             functions: HashMap::new(),
//         }
//     }
// }
