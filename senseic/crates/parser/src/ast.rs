use crate::{const_print::const_assert_eq, lexer::SourceSpan};
use allocator_api2::vec::Vec;
use bumpalo::Bump;
use inturn::Interner;
use neosen_data::{X32, bigint::FrozenBigUint};
use std::convert::Infallible;

pub struct InternedString;
pub type IStr = X32<InternedString>;
pub type StringInterner = Interner<IStr>;
pub type AstBox<'ast, T> = &'ast mut T;

#[derive(Debug)]
pub struct Ast<'ast> {
    pub declarations: Vec<Declaration<'ast>, &'ast Bump>,
}

#[derive(Debug)]
pub struct Declaration<'ast> {
    pub span: SourceSpan,
    pub kind: DeclarationKind<'ast>,
}

#[derive(Debug)]
pub enum DeclarationKind<'ast> {
    Init(Block<'ast>),
    Run(Block<'ast>),
    ConstDef(ConstDef<'ast>),
}

#[derive(Debug)]
pub struct Block<'ast> {
    pub span: SourceSpan,
    pub statements: AstBox<'ast, [Statement<'ast>]>,
    pub last_expr: Option<AstBox<'ast, Expr<'ast>>>,
}

#[derive(Debug)]
pub struct ConstDef<'ast> {
    pub span: SourceSpan,
    pub ident: IStr,
    pub r#type: Option<Expr<'ast>>,
    pub expr: Expr<'ast>,
}

#[derive(Debug)]
pub struct Statement<'ast> {
    pub span: SourceSpan,
    pub kind: StatementKind<'ast>,
}

#[derive(Debug)]
pub enum StatementKind<'ast> {
    Let(AstBox<'ast, LetStmt<'ast>>),
    Return(Expr<'ast>),
    Assign(AstBox<'ast, AssignStmt<'ast>>),
    Block(Block<'ast>),
    Conditional(AstBox<'ast, Conditional<'ast, ()>>),
    While(AstBox<'ast, WhileStmt<'ast>>),
    Expr(Expr<'ast>),
}

#[derive(Debug)]
pub struct WhileStmt<'ast> {
    pub span: SourceSpan,
    pub inline: bool,
    pub condition: Expr<'ast>,
    pub body: Block<'ast>,
}

#[derive(Debug)]
pub struct LetStmt<'ast> {
    pub span: SourceSpan,
    pub mutable: bool,
    pub ident: IStr,
    pub r#type: Option<Expr<'ast>>,
    pub value: Expr<'ast>,
}

#[derive(Debug)]
pub struct AssignStmt<'ast> {
    pub span: SourceSpan,
    pub target: NamePath<'ast>,
    pub op: AssignOp,
    pub value: Expr<'ast>,
}

#[derive(Debug)]
pub struct IntLiteral<'ast> {
    pub span: SourceSpan,
    pub positive: bool,
    pub num: FrozenBigUint<'ast>,
}

#[derive(Debug)]
pub enum TypeDef<'ast> {
    FnDef(AstBox<'ast, FnDef<'ast>>),
    StructDef(StructDef<'ast>),
}

impl<'ast> TypeDef<'ast> {
    pub fn span(&self) -> SourceSpan {
        match self {
            TypeDef::FnDef(fn_def) => fn_def.span,
            TypeDef::StructDef(struct_def) => struct_def.span,
        }
    }
}

#[derive(Debug)]
pub struct FnDef<'ast> {
    pub span: SourceSpan,
    pub params: AstBox<'ast, [ParamDef<'ast>]>,
    pub result: Option<Expr<'ast>>,
    pub body: Block<'ast>,
}

#[derive(Debug)]
pub struct ParamDef<'ast> {
    pub span: SourceSpan,
    pub comptime: bool,
    pub name: IStr,
    pub r#type: Expr<'ast>,
}

#[derive(Debug)]
pub struct FieldDef<'ast> {
    pub span: SourceSpan,
    pub name: IStr,
    pub r#type: Expr<'ast>,
}

#[derive(Debug)]
pub struct StructDef<'ast> {
    pub span: SourceSpan,
    pub fields: AstBox<'ast, [FieldDef<'ast>]>,
}

#[derive(Debug)]
pub struct FieldInit<'ast> {
    pub span: SourceSpan,
    pub name: IStr,
    pub value: Expr<'ast>,
}

#[derive(Debug)]
pub struct StructLiteral<'ast> {
    pub span: SourceSpan,
    pub type_path: NamePath<'ast>,
    pub fields: AstBox<'ast, [FieldInit<'ast>]>,
}

#[derive(Debug)]
pub struct NamePath<'ast> {
    pub span: SourceSpan,
    pub segments: AstBox<'ast, [IStr]>,
}

const _AST_SIZE: () = const {
    const_assert_eq(std::mem::size_of::<Expr<'_>>(), 48);
    const_assert_eq(std::mem::size_of::<ExprKind<'_>>(), 40);
    const_assert_eq(std::mem::size_of::<LetStmt<'_>>(), 112);
    const_assert_eq(std::mem::size_of::<AssignStmt<'_>>(), 80);
    const_assert_eq(std::mem::size_of::<ConstDef<'_>>(), 112);

    const_assert_eq(std::mem::size_of::<Statement<'_>>(), 64);
    const_assert_eq(std::mem::size_of::<StatementKind<'_>>(), 56);
    const_assert_eq(std::mem::size_of::<Block<'_>>(), 32);
    const_assert_eq(std::mem::size_of::<IntLiteral<'_>>(), 32);
    const_assert_eq(std::mem::size_of::<IStr>(), 4);
    const_assert_eq(std::mem::size_of::<Member<'_>>(), 24);
    const_assert_eq(std::mem::size_of::<BinaryExpr<'_>>(), 32);
    const_assert_eq(std::mem::size_of::<FnCall<'_>>(), 32);
    const_assert_eq(std::mem::size_of::<Conditional<'_, ()>>(), 144);
    const_assert_eq(std::mem::size_of::<Declaration<'_>>(), 120);
    const_assert_eq(std::mem::size_of::<DeclarationKind<'_>>(), 112);
    const_assert_eq(std::mem::size_of::<NamePath<'_>>(), 24);
    const_assert_eq(std::mem::size_of::<IfBranch<'_>>(), 88);
};

#[derive(Debug)]
pub struct Spanned<T> {
    pub span: SourceSpan,
    pub inner: T,
}

impl<T> Spanned<T> {
    pub fn new(span: SourceSpan, inner: T) -> Self {
        Self { span, inner }
    }
}

pub type Ident = Spanned<IStr>;

#[derive(Debug)]
pub struct Member<'ast> {
    pub span: SourceSpan,
    pub expr: AstBox<'ast, Expr<'ast>>,
    pub ident: IStr,
}

#[derive(Debug)]
pub struct Expr<'ast> {
    pub span: SourceSpan,
    pub kind: ExprKind<'ast>,
}

#[derive(Debug)]
pub enum ExprKind<'ast> {
    TypeDef(TypeDef<'ast>),
    Block(Block<'ast>),
    Comptime(Block<'ast>),
    Binary(BinaryExpr<'ast>),
    IntLiteral(IntLiteral<'ast>),
    BoolLiteral(bool),
    Ident(IStr),
    Member(Member<'ast>),
    FnCall(FnCall<'ast>),
    Conditional(AstBox<'ast, Conditional<'ast, Infallible>>),
    StructLiteral(AstBox<'ast, StructLiteral<'ast>>),
}

#[derive(Debug)]
pub struct BinaryExpr<'ast> {
    pub span: SourceSpan,
    pub lhs: AstBox<'ast, Expr<'ast>>,
    pub rhs: AstBox<'ast, Expr<'ast>>,
    pub op: BinaryOp,
}

#[derive(Debug)]
pub struct FnCall<'ast> {
    pub span: SourceSpan,
    pub fn_expr: AstBox<'ast, Expr<'ast>>,
    pub param_exprs: AstBox<'ast, [Expr<'ast>]>,
}

#[derive(Debug)]
pub enum MaybeOr<Just, Other> {
    Just(Just),
    Other(Other),
}

#[derive(Debug)]
pub struct Conditional<'ast, ElseMissing> {
    pub span: SourceSpan,
    pub r#if: IfBranch<'ast>,
    pub else_ifs: AstBox<'ast, [IfBranch<'ast>]>,
    pub else_body: MaybeOr<Block<'ast>, ElseMissing>,
}

#[derive(Debug)]
pub struct IfBranch<'ast> {
    pub span: SourceSpan,
    pub condition: Expr<'ast>,
    pub body: Block<'ast>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    // Arithmetic
    AddWrap,
    AddChecked,
    SubWrap,
    SubChecked,
    MulWrap,
    MulChecked,
    DivToPos,
    DivToNeg,
    DivToZero,
    DivFromZero,
    Mod,
    // Comparison
    LessThan,
    LessThanEquals,
    GreaterThan,
    GreaterThanEquals,
    EqualEqual,
    NotEquals,
    // Logical
    LogicalAnd,
    LogicalOr,
    // Bitwise
    BitAnd,
    BitOr,
    BitXor,
    ShiftLeft,
    ShiftRight,
}

#[derive(Debug)]
pub enum AssignOp {
    Assign,
}
