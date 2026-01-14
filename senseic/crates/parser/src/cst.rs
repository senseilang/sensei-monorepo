use allocator_api2::vec::Vec;
use bumpalo::Bump;
use neosen_data::{Span, X32};

pub struct TokenIndex;
pub type TokenIdx = X32<TokenIndex>;

pub struct NodeIndex;
pub type NodeIdx = X32<NodeIndex>;

#[derive(Debug, Clone)]
pub struct Node {
    pub kind: NodeKind,
    pub tokens: Span<TokenIdx>,
    pub next_sibling: Option<NodeIdx>,
    pub first_child: Option<NodeIdx>,
}

#[derive(Debug, Clone, Copy)]
pub enum NodeKind {
    // Declarations
    ConstDecl,
    InitBlock,
    RunBlock,

    Block,
}

#[derive(Debug, Clone)]
pub struct ConcreteSyntaxTree<'ast> {
    pub nodes: Vec<Node, &'ast Bump>,
}
