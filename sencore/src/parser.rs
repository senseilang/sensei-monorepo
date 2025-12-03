use crate::lexer::{Lexer, Token};

use crate::span::Span;
use crate::*;

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
    Void,
    Bool(bool),
    Name(&'src str),
    List(Vec<SNode<'src>>),
}

struct Parser<'src> {
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

impl<'src> Parser<'src> {
    fn new(source: &'src str) -> Self {
        Self {
            tokens: Lexer::new(source).peekable(),
        }
    }

    fn parse_list(&mut self) -> Option<SNode<'src>> {
        let tok = self.tokens.next()?;
        let (Token::RoundOpen, _) = tok else {
            panic!("unexpected {tok:?}")
        };

        let mut children = Vec::new();
        let start_span = self.tokens.peek().expect("unexpected EOF").1;
        let mut end_span;

        loop {
            let (ty, span) = *self.tokens.peek().expect("unexpected EOF");
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
                Token::Percent => {
                    self.tokens.next();
                    children.push(snode!(Void, span));
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
                        self.parse_list()
                            .expect("failed to parse nested list without panic"),
                    );
                }
            }
        }

        Some(snode!(
            List,
            children,
            Span::new(start_span.start, end_span.end)
        ))
    }
}

struct FunctionDef {
    func: AnonFunc,
    params: StructType,
}

struct Definitions<'src> {
    next_type_id: u32,
    types: HashMap<&'src str, Type>,
    functions: HashMap<&'src str, FunctionDef>,
}

impl<'src> Default for Definitions<'src> {
    fn default() -> Self {
        let mut types = HashMap::new();
        types.insert("word", Type::Word);
        types.insert("void", Type::Void);
        types.insert("bool", Type::Bool);
        Self {
            next_type_id: 0,
            types,
            functions: HashMap::new(),
        }
    }
}
