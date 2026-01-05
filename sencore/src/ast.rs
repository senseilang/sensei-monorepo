use crate::comptime_value::{Builtin, Value};

type Span = crate::Span<usize>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Name {
    pub name: Box<str>,
    pub span: Span,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncDef {
    pub recursive_name: Option<Name>,
    pub is_comptime: bool,
    pub func_bind: Name,
    pub bind_type_expr: Expr,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FuncApp {
    pub func_expr: Expr,
    pub applying_expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct LetBind {
    pub span: Span,
    pub is_comptime: bool,
    pub bind_local: Name,
    pub bind_type_expr: Expr,
    pub assigned: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FixBind {
    pub name: Name,
    pub expr: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructField {
    pub span: Span,
    pub name: Name,
    pub r#type: Expr,
}

pub type StructDefUniqueId = u32;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructDef {
    pub def_uuid: StructDefUniqueId,
    pub fields: Vec<StructField>,
    pub capture: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructInitField {
    pub span: Span,
    pub name: Name,
    pub value: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct StructInit {
    pub struct_type: Expr,
    pub fields: Vec<StructInitField>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IfThenElse {
    pub condition: Expr,
    pub true_branch: Expr,
    pub false_branch: Expr,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct MemberAccess {
    pub r#struct: Expr,
    pub member: Name,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BuiltinCall {
    pub builtin: Builtin,
    pub arguments: Vec<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ExprKind {
    Var(Box<str>),
    Value(Box<Value>),

    BuiltinCall(Box<BuiltinCall>),
    MemberAccess(Box<MemberAccess>),
    IfThenElse(Box<IfThenElse>),
    FuncApp(Box<FuncApp>),
    FuncDef(Box<FuncDef>),
    StructDef(Box<StructDef>),
    StructInit(Box<StructInit>),
}

impl<T> From<T> for ExprKind
where
    T: Into<Value>,
{
    fn from(value: T) -> Self {
        Self::Value(Box::new(value.into()))
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Expr {
    pub span: Span,
    pub kind: ExprKind,
}

impl std::ops::Deref for Expr {
    type Target = ExprKind;

    fn deref(&self) -> &Self::Target {
        &self.kind
    }
}

#[derive(Debug)]
pub struct Ast {
    pub runtime_main: Expr,
}
