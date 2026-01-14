pub mod bigint;
pub mod index;
pub mod span;

pub use crate::{index::X32, span::Span};

/// Core crate assumption.
const _USIZE_AT_LEAST_U32: () = const {
    assert!(u32::BITS <= usize::BITS);
};
