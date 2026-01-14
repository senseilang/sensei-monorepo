use crate::{
    cst::{self, *},
    diagnostics::DiagnosticsContext,
    lexer::*,
    parser::token_item_iter::TokenItems,
};
use allocator_api2::vec::Vec;
use bumpalo::Bump;
use neosen_data::Span;

type TokenItem = (Token, TokenIdx, SourceSpan);

// Ensure fields are private letting us more easily enforce the invariant that the iterator either
// consumes fuel or advances.
mod token_item_iter {
    use super::TokenItem;
    use std::iter::Peekable;

    pub(super) struct TokenItems<TokenIter: Iterator<Item = TokenItem>> {
        tokens: Peekable<TokenIter>,
        fuel: u32,
    }

    impl<TokenIter: Iterator<Item = TokenItem>> TokenItems<TokenIter> {
        const DEFAULT_FUEL: u32 = 256;

        pub(crate) fn new(tokens: TokenIter) -> TokenItems<TokenIter> {
            TokenItems { tokens: tokens.peekable(), fuel: Self::DEFAULT_FUEL }
        }

        pub(crate) fn peek(&mut self) -> Option<TokenItem> {
            self.fuel -= 1;
            assert!(self.fuel > 0, "out of fuel");
            self.tokens.peek().copied()
        }

        pub(super) fn next(&mut self) -> Option<TokenItem> {
            self.fuel = Self::DEFAULT_FUEL;
            self.tokens.next()
        }
    }
}

struct Parser<'ast, TokenIter: Iterator<Item = TokenItem>, D: DiagnosticsContext> {
    nodes: Vec<cst::Node, &'ast Bump>,
    expected: Vec<Token, &'ast Bump>,
    tokens: TokenItems<TokenIter>,
    diagnostics: D,
}

impl<'ast, TokenIter, D> Parser<'ast, TokenIter, D>
where
    D: DiagnosticsContext,
    TokenIter: Iterator<Item = TokenItem>,
{
    fn new(
        arena: &'ast Bump,
        tokens: TokenIter,
        estimated_node_count: usize,
        diagnostics: D,
    ) -> Self {
        Parser {
            tokens: TokenItems::new(tokens),
            nodes: Vec::with_capacity_in(estimated_node_count, arena),
            expected: Vec::with_capacity_in(8, arena),
            diagnostics,
        }
    }

    fn alloc_node(&mut self, kind: NodeKind) -> NodeIdx {
        let id = NodeIdx::new(self.nodes.len() as u32);
        self.nodes.push(Node {
            kind,
            tokens: Span::new(TokenIdx::ZERO, TokenIdx::ZERO),
            next_sibling: None,
            first_child: None,
        });
        id
    }

    fn current(&mut self) -> Option<TokenItem> {
        self.tokens.peek()
    }

    fn advance(&mut self) -> Option<TokenItem> {
        let item = self.tokens.next();

        if let Some((token, ti, src_span)) = item
            && token.is_error()
        {
            self.diagnostics.emit_lexer_error(token, ti, src_span);
        }

        debug_assert!(item.is_some(), "no advance");
        item
    }

    fn at(&mut self, token: Token) -> bool {
        self.current().is_some_and(|(next_token, _, _)| next_token == token)
    }

    fn check(&mut self, token: Token) -> bool {
        if self.at(token) {
            return true;
        }
        self.expected.push(token);
        false
    }

    fn eat(&mut self, token: Token) -> bool {
        let present = self.check(token);
        if present {
            self.advance();
        }
        present
    }

    fn eof(&mut self) -> bool {
        self.current().is_none()
    }

    fn advance_filtered(&mut self, filter_bad_ident: bool) -> Option<TokenItem> {
        while let Some((token, ti, src_span)) = self.current() {
            if token.is_trivia() {
                self.advance();
                continue;
            }
            if token.is_error() && (filter_bad_ident || token != Token::MalformedIdentError) {
                self.advance();
                continue;
            }

            return Some((token, ti, src_span));
        }

        None
    }

    fn parse_program(&mut self) {
        let mut last: TokenIdx = TokenIdx::ZERO;

        while let Some((token, _, _)) = self.advance_filtered(true) {
            if self.check(Token::Init) || self.check(Token::Run) {
                self.advance();
                last = self.parse_block(last, Some(token));
            } else {
                todo!("handle unexpected token error")
            }
        }
    }

    fn parse_block(&mut self, start: TokenIdx, block_token: Option<Token>) -> TokenIdx {
        let block_kind = match block_token {
            Some(Token::Init) => NodeKind::InitBlock,
            Some(Token::Run) => NodeKind::RunBlock,
            None => NodeKind::Block,
            Some(invalid) => unreachable!("invalid block kind {:?}", invalid),
        };
        let block = self.alloc_node(block_kind);

        while let Some((token, _, _)) = self.advance_filtered(true) {
            if self.eat(Token::LeftCurly) {
                break;
            } else if token == Token::RightCurly {
                todo!("premature close")
            } else {
                todo!("handle unexpected token error")
            }
        }

        todo!()
    }
}

pub fn parse<'ast, 'src, D: DiagnosticsContext>(
    arena: &'ast Bump,
    lexer: Lexer<'src>,
    estimated_node_count: usize,
    diagnostics: D,
) -> ConcreteSyntaxTree<'ast> {
    let mut parser = Parser::new(
        arena,
        lexer.enumerate().map(|(i, (tok, span))| (tok, TokenIdx::new(i as u32), span)),
        estimated_node_count,
        diagnostics,
    );

    parser.parse_program();

    ConcreteSyntaxTree { nodes: parser.nodes }
}
