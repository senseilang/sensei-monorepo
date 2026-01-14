use crate::{
    cst::TokenIdx,
    lexer::{SourceSpan, Token},
};

pub trait DiagnosticsContext {
    fn emit_lexer_error(&mut self, token: Token, index: TokenIdx, src_span: SourceSpan);
}
