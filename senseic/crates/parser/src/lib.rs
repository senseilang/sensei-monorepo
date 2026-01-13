pub mod ast;
pub mod diagnostics;
mod display;
pub mod lexer;
pub mod parser;

pub mod const_print;

pub use ast::StringInterner;
pub use display::AstDisplay;

/// Core crate assumption.
const _USIZE_AT_LEAST_U32: () = const {
    assert!(u32::BITS <= usize::BITS);
};
