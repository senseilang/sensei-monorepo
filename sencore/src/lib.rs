pub mod ast;
pub mod lexer;
pub mod parser;
pub mod span;

pub use span::Span;

use std::collections::HashMap;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Type {
    Word,
    Void,
    Bool,
    Struct(StructType),
    Enum(EnumType),
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct StructType {
    pub id: u32,
    pub field_types: Vec<Type>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct EnumType {
    pub id: u32,
    pub variants: Vec<Type>,
}

pub type WordValue = i32;

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub enum Value {
    Void,
    Word(WordValue),
    Bool(bool),
    Struct(StructValue),
    Enum(EnumValue),
}

impl Value {
    fn to_type(&self) -> Type {
        match self {
            Self::Void => Type::Void,
            Self::Word(_) => Type::Word,
            Self::Bool(_) => Type::Bool,
            Self::Struct(r#struct) => Type::Struct(r#struct.to_type()),
            Self::Enum(r#enum) => Type::Enum(r#enum.r#type.clone()),
        }
    }

    fn word_or_none(&self) -> Option<WordValue> {
        match self {
            Self::Word(x) => Some(*x),
            _ => None,
        }
    }

    fn bool_or_none(&self) -> Option<bool> {
        match self {
            Self::Bool(b) => Some(*b),
            _ => None,
        }
    }

    fn struct_or_none(self) -> Option<StructValue> {
        match self {
            Self::Struct(x) => Some(x),
            _ => None,
        }
    }

    fn enum_or_none(self) -> Option<EnumValue> {
        match self {
            Self::Enum(x) => Some(x),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct StructValue {
    pub id: u32,
    pub fields: Vec<Value>,
}

impl StructValue {
    fn to_type(&self) -> StructType {
        StructType {
            id: self.id,
            field_types: self.fields.iter().map(Value::to_type).collect(),
        }
    }
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct EnumValue {
    r#type: EnumType,
    variant: usize,
    value: Box<Value>,
}

#[derive(Debug, Clone, Hash)]
pub enum Expr {
    ConstWord(WordValue),
    Add(Box<Expr>, Box<Expr>),
    GreaterThan(Box<Expr>, Box<Expr>),
    ConstBool(bool),
    ConstVoid,
    BindingRef(BindingId),
    Let(LetExpression),
    StructLiteral(StructLiteral),
    EnumLiteral(EnumLiteral),
    StructMemberAccess(StructMemberAccess),
    EnumMatch(EnumMatch),
    IfThenElse(IfThenElse),
    FunctionInvocation(FunctionInvocation),
}

pub type BindingId = u32;

#[derive(Debug, Clone, Hash)]
pub struct LetExpression {
    pub binding: BindingId,
    pub assigned: Box<Expr>,
    pub r#in: Box<Expr>,
}

#[derive(Debug, Clone, Hash)]
pub struct StructLiteral {
    pub r#type: StructType,
    pub values: Vec<Expr>,
}

#[derive(Debug, Clone, Hash)]
pub struct EnumLiteral {
    pub r#type: EnumType,
    pub variant: usize,
    pub value: Box<Expr>,
}

#[derive(Debug, Clone, Hash)]
pub struct StructMemberAccess {
    pub r#struct: Box<Expr>,
    pub member: usize,
}

#[derive(Debug, Clone, Hash)]
pub struct EnumMatch {
    pub matching: Box<Expr>,
    pub match_arms: Vec<MatchArm>,
}

#[derive(Debug, Clone, Hash)]
pub struct MatchArm {
    pub binds_variant_value_to: BindingId,
    pub body: Expr,
}

#[derive(Debug, Clone, Hash)]
pub struct IfThenElse {
    pub condition: Box<Expr>,
    pub true_branch: Box<Expr>,
    pub false_branch: Box<Expr>,
}

pub type FunctionId = u32;

#[derive(Debug, Clone, Hash)]
pub struct FunctionInvocation {
    pub function: FunctionId,
    pub input: Box<Expr>,
}

#[derive(Debug, Clone, Hash)]
pub struct AnonFunc {
    pub input_bind: BindingId,
    pub input: Type,
    pub output: Type,
    pub body: Expr,
}

#[derive(Debug, Clone)]
pub struct TypingContext<'f> {
    pub functions: &'f HashMap<FunctionId, AnonFunc>,
    pub bound: HashMap<BindingId, Type>,
}

impl<'f> TypingContext<'f> {
    fn new(functions: &'f HashMap<FunctionId, AnonFunc>) -> Self {
        Self {
            functions,
            bound: HashMap::new(),
        }
    }

    pub fn bind(&mut self, id: BindingId, r#type: Type) -> Result<(), String> {
        if self.bound.insert(id, r#type).is_some() {
            Err(format!("Rebinding #{}", id))
        } else {
            Ok(())
        }
    }
}

pub fn type_check_expr(ctx: &mut TypingContext, expr: &Expr) -> Result<Type, String> {
    use Expr as E;
    match expr {
        E::ConstWord(_) => Ok(Type::Word),
        E::Add(lhs, rhs) => {
            let lhs = type_check_expr(ctx, lhs)?;
            if lhs != Type::Word {
                return Err(format!("add: typeOf(lhs) == {:?}, expected: word", lhs));
            }
            let rhs = type_check_expr(ctx, rhs)?;
            if rhs != Type::Word {
                return Err(format!("add: typeOf(rhs) == {:?}, expected: word", rhs));
            }
            Ok(Type::Word)
        }
        E::GreaterThan(lhs, rhs) => {
            let lhs = type_check_expr(ctx, lhs)?;
            if lhs != Type::Word {
                return Err(format!("gt: typeOf(lhs) == {:?}, expected: word", lhs));
            }
            let rhs = type_check_expr(ctx, rhs)?;
            if rhs != Type::Word {
                return Err(format!("gt: typeOf(rhs) == {:?}, expected: word", rhs));
            }
            Ok(Type::Bool)
        }
        E::ConstBool(_) => Ok(Type::Bool),
        E::ConstVoid => Ok(Type::Void),
        E::BindingRef(id) => ctx
            .bound
            .get(id)
            .cloned()
            .ok_or_else(|| format!("Referenced unbound #{}", id)),
        E::Let(r#let) => {
            let r#type = type_check_expr(ctx, &r#let.assigned)?;
            ctx.bind(r#let.binding, r#type)?;
            type_check_expr(ctx, &r#let.r#in)
        }
        E::StructLiteral(lit) => {
            let r#type = lit.r#type.clone();
            if r#type.field_types.len() != lit.values.len() {
                return Err(format!(
                    "Mismatched field count type != lit: {} != {}",
                    r#type.field_types.len(),
                    lit.values.len()
                ));
            }
            for (i, (field, value)) in r#type.field_types.iter().zip(&lit.values).enumerate() {
                let value_type = type_check_expr(ctx, value)?;
                if field != &value_type {
                    return Err(format!(
                        "Type mismatch in field #{i} of struct literal, expected != got: {field:?} != {value_type:?}"
                    ));
                }
            }
            Ok(Type::Struct(r#type))
        }
        E::EnumLiteral(lit) => {
            let r#type = lit.r#type.clone();
            let variant_type = &r#type.variants[lit.variant];
            let value_type = type_check_expr(ctx, &lit.value)?;
            if variant_type != &value_type {
                return Err(format!(
                    "Type mismatch for variant #{} in enum literal, expected != got: {variant_type:?} != {value_type:?}",
                    lit.variant
                ));
            }
            Ok(Type::Enum(r#type))
        }
        E::StructMemberAccess(member_access) => {
            let r#struct = type_check_expr(ctx, &member_access.r#struct)?;
            let Type::Struct(struct_type) = r#struct else {
                return Err(format!("Member access on non-struct expression"));
            };
            struct_type
                .field_types
                .get(member_access.member)
                .cloned()
                .ok_or_else(|| {
                    format!(
                        "Invalid field #{}, struct only has {} members",
                        member_access.member,
                        struct_type.field_types.len()
                    )
                })
        }
        E::EnumMatch(r#match) => {
            let r#enum = type_check_expr(ctx, &r#match.matching)?;
            let Type::Enum(r#enum) = r#enum else {
                return Err(format!("Match on non-enum expression"));
            };
            if r#enum.variants.len() != r#match.match_arms.len() {
                return Err(format!(
                    "Mismatched enum variant & match arm count: {} != {}",
                    r#enum.variants.len(),
                    r#match.match_arms.len()
                ));
            }
            let mut match_arm_types = r#enum
                .variants
                .iter()
                .zip(&r#match.match_arms)
                .enumerate()
                .map(|(i, (variant, arm))| -> Result<(Type, usize), String> {
                    ctx.bind(arm.binds_variant_value_to, variant.clone())?;
                    let arm_result = type_check_expr(ctx, &arm.body)?;
                    Ok((arm_result, i))
                });
            let (first_arm_res_type, _) = match_arm_types.next().expect("zero match arms")?;
            while let Some((arm_res_type, i)) = match_arm_types.next().transpose()? {
                if arm_res_type != first_arm_res_type {
                    return Err(format!("Match arm result type mismatch, #0 != #{i}"));
                }
            }
            Ok(first_arm_res_type)
        }
        E::IfThenElse(if_then_else) => {
            let condition = type_check_expr(ctx, &if_then_else.condition)?;
            if condition != Type::Bool {
                return Err(format!("Condition type not bool"));
            }
            let r#true = type_check_expr(ctx, &if_then_else.true_branch)?;
            let r#false = type_check_expr(ctx, &if_then_else.false_branch)?;
            if r#true != r#false {
                return Err(format!("If-Else branch result type mismatches"));
            }
            Ok(r#true)
        }
        E::FunctionInvocation(application) => {
            let input = type_check_expr(ctx, &application.input)?;

            let func = &ctx.functions[&application.function];
            if func.input != input {
                return Err(format!(
                    "Function application parameter type doesn't match input"
                ));
            }
            Ok(func.output.clone())
        }
    }
}
#[derive(Debug, Clone)]
pub struct EvaluationContext<'f> {
    functions: &'f HashMap<FunctionId, AnonFunc>,
    bound: HashMap<BindingId, Value>,
}

impl<'f> EvaluationContext<'f> {
    pub fn new(functions: &'f HashMap<FunctionId, AnonFunc>) -> Self {
        Self {
            functions,
            bound: HashMap::new(),
        }
    }

    pub fn bind(&mut self, id: BindingId, value: Value) {
        assert!(self.bound.insert(id, value).is_none(), "Rebinding #{}", id);
    }
}

pub fn evaluate_expression(ctx: &mut EvaluationContext, expr: &Expr) -> Value {
    use Expr as E;
    match expr {
        E::ConstWord(value) => Value::Word(*value),
        E::Add(lhs, rhs) => {
            let lhs = evaluate_expression(ctx, lhs)
                .word_or_none()
                .expect("type-safety violated");
            let rhs = evaluate_expression(ctx, rhs)
                .word_or_none()
                .expect("type-safety violated");
            Value::Word(lhs.wrapping_add(rhs))
        }
        E::GreaterThan(lhs, rhs) => {
            let lhs = evaluate_expression(ctx, lhs)
                .word_or_none()
                .expect("type-safety violated");
            let rhs = evaluate_expression(ctx, rhs)
                .word_or_none()
                .expect("type-safety violated");
            Value::Bool(lhs > rhs)
        }
        E::ConstBool(b) => Value::Bool(*b),
        E::ConstVoid => Value::Void,
        E::BindingRef(id) => ctx.bound[id].clone(),
        E::Let(r#let) => {
            let assigning = evaluate_expression(ctx, &r#let.assigned);
            ctx.bind(r#let.binding, assigning);
            evaluate_expression(ctx, &r#let.r#in)
        }
        E::StructLiteral(lit) => {
            let fields = lit
                .values
                .iter()
                .map(|field| evaluate_expression(ctx, field))
                .collect();
            Value::Struct(StructValue {
                id: lit.r#type.id,
                fields,
            })
        }
        E::EnumLiteral(lit) => {
            let value = evaluate_expression(ctx, &lit.value);
            Value::Enum(EnumValue {
                r#type: lit.r#type.clone(),
                variant: lit.variant,
                value: Box::new(value),
            })
        }
        E::StructMemberAccess(member_access) => {
            let mut value = evaluate_expression(ctx, &member_access.r#struct)
                .struct_or_none()
                .expect("type-safety violated");
            value.fields.swap_remove(member_access.member)
        }
        E::EnumMatch(r#match) => {
            let value = evaluate_expression(ctx, &r#match.matching)
                .enum_or_none()
                .expect("type-safety violated");
            let arm = &r#match.match_arms[value.variant];
            ctx.bind(
                arm.binds_variant_value_to,
                <Value as Clone>::clone(&value.value),
            );
            evaluate_expression(ctx, &arm.body)
        }
        E::IfThenElse(if_else) => {
            let value = evaluate_expression(ctx, &if_else.condition)
                .bool_or_none()
                .expect("type-safety violated");
            if value {
                evaluate_expression(ctx, &if_else.true_branch)
            } else {
                evaluate_expression(ctx, &if_else.false_branch)
            }
        }
        E::FunctionInvocation(fn_invocation) => {
            let value = evaluate_expression(ctx, &fn_invocation.input);
            let mut new_call_ctx = EvaluationContext::new(ctx.functions);
            let func = &ctx.functions[&fn_invocation.function];
            new_call_ctx.bind(func.input_bind, value);
            evaluate_expression(&mut new_call_ctx, &func.body)
        }
    }
}
