use logos::{Lexer as LogosLexer, Logos, Skip};

use neosen_data::Span;
pub type SourceSpan = Span<u32>;

fn lex_skip_line_comment(lex: &mut LogosLexer<Token>) {
    let remainder = lex.remainder();
    let mut chars = remainder.char_indices().peekable();
    while chars.next_if(|&(_, c)| c != '\n').is_some() {}
    let bytes_skipped = chars.peek().map_or(remainder.len(), |&(pos, _)| pos);
    lex.bump(bytes_skipped);
}

fn lex_skip_block_comment(lex: &mut LogosLexer<Token>) -> Result<Skip, ()> {
    let remainder = lex.remainder();
    let mut chars = remainder.char_indices().peekable();
    let mut depth: u32 = 1;

    while depth > 0 {
        match chars.next() {
            Some((_, '*')) if chars.next_if(|&(_, nc)| nc == '/').is_some() => depth -= 1,
            Some((_, '/')) if chars.next_if(|&(_, nc)| nc == '*').is_some() => depth += 1,
            Some(_) => {}
            None => return Err(()),
        }
    }

    let bytes_skipped = chars.peek().map_or(remainder.len(), |&(pos, _)| pos);
    lex.bump(bytes_skipped);

    Ok(Skip)
}

fn lex_string_literal(lex: &mut LogosLexer<Token>) -> Result<(), ()> {
    let remainder = lex.remainder();
    let mut chars = remainder.char_indices();

    loop {
        match chars.next() {
            Some((_, '\\')) => {
                // Skip the escaped character
                if chars.next().is_none() {
                    return Err(());
                }
            }
            Some((pos, '"')) => {
                lex.bump(pos + 1);
                return Ok(());
            }
            Some(_) => {}
            None => return Err(()),
        }
    }
}

#[derive(Logos, Debug, Clone, PartialEq, Eq, Copy)]
#[logos(skip r"[ \t]+")]
#[logos(skip(r"//", lex_skip_line_comment))]
#[logos(skip(r"/\*", lex_skip_block_comment))]
pub enum Token {
    // Delimiters
    #[token(";")]
    Semicolon,
    #[token(",")]
    Comma,
    #[token(":")]
    Colon,
    #[token(".")]
    Dot,
    #[token("{")]
    LeftCurly,
    #[token("}")]
    RightCurly,
    #[token("(")]
    LeftRound,
    #[token(")")]
    RightRound,
    #[token("[")]
    LeftSquare,
    #[token("]")]
    RightSquare,

    // Operators
    #[token("->")]
    ThinArrow,
    #[token("=")]
    Equals,
    #[token("+")]
    Plus,
    #[token("-")]
    Minus,
    #[token("*")]
    Star,
    #[token("/")]
    Slash,
    #[token("%")]
    Percent,

    // Comparison
    #[token("==")]
    DoubleEquals,
    #[token("!=")]
    NotEquals,
    #[token("<")]
    LessThan,
    #[token(">")]
    GreaterThan,
    #[token("<=")]
    LessEquals,
    #[token(">=")]
    GreaterEquals,

    // Logical
    #[token("&&")]
    And,
    #[token("||")]
    Or,
    #[token("!")]
    Not,

    // Bitwise
    #[token("&")]
    Ampersand,
    #[token("|")]
    Pipe,
    #[token("^")]
    Caret,
    #[token("~")]
    Tilde,
    #[token("<<")]
    ShiftLeft,
    #[token(">>")]
    ShiftRight,

    // Keywords
    #[token("if")]
    If,
    #[token("else")]
    Else,
    #[token("fn")]
    Fn,
    #[token("let")]
    Let,
    #[token("mut")]
    Mut,
    #[token("const")]
    Const,
    #[token("init")]
    Init,
    #[token("run")]
    Run,
    #[token("struct")]
    Struct,
    #[token("import")]
    Import,
    #[token("from")]
    From,
    #[token("as")]
    As,
    #[token("export")]
    Export,
    #[token("return")]
    Return,
    #[token("true")]
    True,
    #[token("false")]
    False,

    #[regex("[a-zA-Z_][a-zA-Z0-9_]*")]
    Identifier,

    #[regex("-?[0-9][0-9_]*")]
    DecLiteral,
    #[regex("-?0x[0-9A-Fa-f][0-9A-Fa-f_]*")]
    HexLiteral,
    #[regex("-?0b[01][01_]*")]
    BinLiteral,
    #[token("\"", lex_string_literal)]
    StringLiteral,

    Error,
}

#[derive(Debug, Clone)]
pub struct Lexer<'src>(LogosLexer<'src, Token>);

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Self {
        Self(Token::lexer(source))
    }
}

impl<'src> Iterator for Lexer<'src> {
    type Item = (Token, std::ops::Range<usize>);

    fn next(&mut self) -> Option<Self::Item> {
        let tok = self.0.next()?.unwrap_or(Token::Error);
        Some((tok, self.0.span()))
    }
}
