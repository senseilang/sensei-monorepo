use crate::Span;
use crate::ast::*;
use crate::comptime_value::*;

pub fn interpret(runtime: &Expr) -> Result<Expr, InterpretError> {
    let mut interpreter = Interpreter::new();

    let out = interpreter.partial_eval(runtime);
    println!("Scope tree:\n{}", interpreter.fmt_scope_tree());

    out
}

#[derive(Debug, Clone)]
pub struct InterpretError {
    pub message: String,
    pub spans: Vec<Span<usize>>,
}

impl InterpretError {
    pub fn new(message: impl Into<String>, span: Span<usize>) -> Self {
        Self {
            message: message.into(),
            spans: vec![span],
        }
    }

    pub fn naked(message: impl Into<String>) -> Self {
        Self {
            message: message.into(),
            spans: vec![],
        }
    }
}

pub trait PushErrSpan<T> {
    fn push_err_span(self, span: Span<usize>) -> Result<T, InterpretError>;
}

impl<T> PushErrSpan<T> for Result<T, InterpretError> {
    fn push_err_span(self, span: Span<usize>) -> Result<T, InterpretError> {
        self.map_err(|mut e| {
            e.spans.push(span);
            e
        })
    }
}

#[derive(Debug, Clone)]
struct Scope {
    parent: ScopeId,
    bind: Box<str>,
    value: ValueOrFree,
}

#[derive(Debug, Clone)]
enum ValueOrFree {
    Value(Value),
    Free,
}

#[derive(Debug, Clone)]
struct Interpreter {
    scope_tree: Vec<Scope>,
    current_scope: ScopeId,
    comptime_memory: Vec<Box<[i32]>>,
}

fn short_fmt(value: impl std::fmt::Debug, max_len: usize) -> String {
    let str = format!("{:?}", value);
    if str.len() > max_len {
        format!("{}...", &str[..max_len])
    } else {
        str
    }
}

impl Interpreter {
    const ROOT_SCOPE: ScopeId = ScopeId(0);

    fn new() -> Self {
        let scope_tree = Vec::with_capacity(128);
        let mut i = Self {
            scope_tree,
            current_scope: Self::ROOT_SCOPE,
            comptime_memory: vec![],
        };

        // Types
        i.bind("void", Type::Void);
        i.bind("i32", Type::Num);
        i.bind("bool", Type::Bool);
        i.bind("memptr", Type::MemoryPointer);
        i.bind("type", Type::Type);
        i.bind("fn", Type::Function);

        i
    }

    fn bind(&mut self, name: impl AsRef<str>, value: impl Into<Value>) {
        self.scope_tree.push(Scope {
            parent: self.current_scope,
            bind: name.as_ref().into(),
            value: ValueOrFree::Value(value.into()),
        });
        self.current_scope = ScopeId(self.scope_tree.len());
    }

    fn bind_free(&mut self, name: impl AsRef<str>) {
        let parent = self.current_scope;
        self.scope_tree.push(Scope {
            parent,
            bind: name.as_ref().into(),
            value: ValueOrFree::Free,
        });
        self.current_scope = ScopeId(self.scope_tree.len());
    }

    fn get(&self, name: &str) -> Option<&ValueOrFree> {
        let mut scope_id = self.current_scope;
        while scope_id != Self::ROOT_SCOPE {
            let scope = &self.scope_tree[scope_id.0 - 1];
            if name == &scope.bind as &str {
                return Some(&scope.value);
            }
            scope_id = scope.parent;
        }
        return None;
    }

    fn fmt_scope_tree(&self) -> String {
        use std::fmt::Write;
        let mut output = String::new();

        for (i, scope) in self.scope_tree.iter().enumerate().rev() {
            writeln!(
                output,
                "  [{}] {:?} = {} (parent: {})",
                i + 1,
                &scope.bind as &str,
                short_fmt(&scope.value, 60),
                scope.parent.0
            )
            .unwrap();
        }

        output
    }

    fn fmt_scope(&self, mut id: ScopeId) -> String {
        use std::fmt::Write;
        let mut output = String::new();

        while id != Self::ROOT_SCOPE {
            let scope = &self.scope_tree[id.0 - 1];
            writeln!(
                output,
                "  [{}] {:?} = {} (parent: {})",
                id.0,
                &scope.bind as &str,
                short_fmt(&scope.value, 60),
                scope.parent.0
            )
            .unwrap();
            id = scope.parent;
        }

        output
    }

    fn comptime_eval(&mut self, expr: &Expr) -> Result<Value, InterpretError> {
        let span = expr.span;
        let v = match &expr.kind {
            ExprKind::Value(v) => Value::clone(v),
            ExprKind::Var(name) => match self.get(&name) {
                Some(ValueOrFree::Value(v)) => v.clone(),
                Some(ValueOrFree::Free) => {
                    return Err(InterpretError::new(
                        format!("Comptime expression references free variable {:?}", name),
                        span,
                    ));
                }
                None => {
                    return Err(InterpretError::new(
                        format!("Undefined reference {:?}", name),
                        span,
                    ));
                }
            },
            ExprKind::IfThenElse(if_else) => {
                let cond = self
                    .comptime_eval(&if_else.condition)
                    .push_err_span(span)?
                    .as_bool()
                    .map_err(|cond| {
                        InterpretError::new(format!("Expected boolean, got: {:?}", cond), span)
                    })?;
                if cond {
                    self.comptime_eval(&if_else.true_branch)
                        .push_err_span(span)?
                } else {
                    self.comptime_eval(&if_else.false_branch)
                        .push_err_span(span)?
                }
            }
            ExprKind::FuncDef(func_def) => {
                let type_value = self.comptime_eval(&func_def.bind_type_expr)?;
                let r#type =
                    Self::as_type(type_value).push_err_span(func_def.bind_type_expr.span)?;

                Closure {
                    r#type,
                    is_comptime: func_def.is_comptime,
                    recursive_name: func_def.recursive_name.clone().map(|name| name.name),
                    binds: func_def.bind.name.clone(),
                    body: func_def.body.clone(),
                    captures: self.current_scope,
                }
                .into()
            }
            ExprKind::FuncApp(func_app) => {
                let func = self.comptime_eval(&func_app.func_expr)?;
                let apply = self.comptime_eval(&func_app.applying_expr)?;
                let closure = func.as_closure().map_err(|non_func| {
                    InterpretError::new(format!("Expected function, got: {:?}", non_func), span)
                })?;
                self.eval_closure(*closure, apply).push_err_span(span)?
            }
            ExprKind::StructDef(struct_def) => {
                let capture = self.comptime_eval(&struct_def.capture)?;
                let fields = struct_def
                    .fields
                    .iter()
                    .map(|field| {
                        let type_value = self.comptime_eval(&field.r#type)?;
                        let r#type = Self::as_type(type_value).push_err_span(field.span)?;
                        Ok((field.name.name.clone(), r#type))
                    })
                    .collect::<Result<_, _>>()?;
                Value::Type(Box::new(Type::Struct(StructType {
                    def_uuid: struct_def.def_uuid,
                    fields,
                    capture,
                })))
            }
            ExprKind::BuiltinCall(call) => self.eval_builtin(call).push_err_span(span)?,
            ExprKind::StructInit(struct_init) => {
                // Evaluate struct_type expression to get Type::Struct
                let struct_type = self
                    .comptime_eval(&struct_init.struct_type)?
                    .as_type()
                    .map_err(|v| {
                        InterpretError::new(
                            format!("Expected type, got: {:?}", v.get_type()),
                            struct_init.struct_type.span,
                        )
                    })?
                    .as_struct()
                    .map_err(|t| {
                        InterpretError::new(
                            format!("Expected struct type, got: {:?}", t),
                            struct_init.struct_type.span,
                        )
                    })?;

                // Check field count matches
                if struct_init.fields.len() != struct_type.fields.len() {
                    return Err(InterpretError::new(
                        format!(
                            "Struct init has {} fields, but struct type has {} fields",
                            struct_init.fields.len(),
                            struct_type.fields.len()
                        ),
                        span,
                    ));
                }

                // Evaluate fields in order, checking names match definition order
                let field_values = struct_init
                    .fields
                    .iter()
                    .zip(struct_type.fields.iter())
                    .map(|(init_field, (expected_name, expected_type))| {
                        // Check field name matches expected order
                        if &*init_field.name.name != &**expected_name {
                            return Err(InterpretError::new(
                                format!(
                                    "Expected field {:?}, got {:?} (fields must be in definition order)",
                                    expected_name, init_field.name.name
                                ),
                                init_field.span,
                            ));
                        }

                        let value = self
                            .comptime_eval(&init_field.value)
                            .push_err_span(init_field.span)?;

                        // Type check
                        let actual_type = value.get_type();
                        if actual_type != *expected_type {
                            return Err(InterpretError::new(
                                format!(
                                    "Field {:?} type mismatch: expected {:?}, got {:?}",
                                    init_field.name.name, expected_type, actual_type
                                ),
                                init_field.span,
                            ));
                        }

                        Ok(value)
                    })
                    .collect::<Result<Vec<_>, _>>()?;

                Value::Struct(Box::new(StructValue {
                    r#type: Type::Struct(struct_type),
                    field_values,
                }))
            }
            ExprKind::MemberAccess(access) => {
                let struct_value =
                    self.comptime_eval(&access.r#struct)?
                        .as_struct()
                        .map_err(|v| {
                            InterpretError::new(
                                format!("Expected struct, got: {:?}", v.get_type()),
                                access.r#struct.span,
                            )
                        })?;

                let struct_type = struct_value
                    .r#type
                    .clone()
                    .as_struct()
                    .expect("StructValue always has Type::Struct");

                let field_index = struct_type
                    .fields
                    .iter()
                    .position(|(name, _)| &**name == &*access.member.name)
                    .ok_or_else(|| {
                        InterpretError::new(
                            format!("Unknown field {:?}", access.member.name),
                            access.member.span,
                        )
                    })?;

                struct_value.field_values[field_index].clone()
            }
        };

        Ok(v)
    }

    fn as_type(value: Value) -> Result<Type, InterpretError> {
        value.as_type().map(|t| *t).map_err(|non_type| {
            InterpretError::naked(format!(
                "Expected closure type expression to evaluate to type, got: {:?}",
                non_type.get_type()
            ))
        })
    }

    fn eval_closure(&mut self, closure: Closure, apply: Value) -> Result<Value, InterpretError> {
        let apply_type = apply.get_type();
        if apply_type != closure.r#type {
            return Err(InterpretError::naked(format!(
                "Type mismatch: expected {:?}, got: {:?}",
                apply_type, closure.r#type
            )));
        }

        let return_scope = self.current_scope;
        self.current_scope = closure.captures;
        self.bind(&closure.binds, apply);
        if let Some(recurse_bind) = &closure.recursive_name {
            self.bind(recurse_bind, Value::Closure(Box::new(closure.clone())));
        }

        let body_res = self.comptime_eval(&closure.body);
        self.current_scope = return_scope;

        body_res
    }

    fn eval_builtin(&mut self, call: &BuiltinCall) -> Result<Value, InterpretError> {
        let mut values = call.arguments.iter().map(|arg| self.comptime_eval(arg));
        let v = match call.builtin {
            Builtin::IsStruct => {
                let r#type = values.next().expect("missing bcall arg")?;
                let r#type = r#type.as_type().map_err(|non_type| {
                    InterpretError::naked(format!(
                        "Cannot determine struct-ness of non-type value <{:?}>",
                        non_type.get_type()
                    ))
                })?;
                Value::Bool(r#type.is_struct())
            }
            Builtin::GetTotalStructFields => {
                let value = values.next().expect("missing bcall arg")?;
                let r#type = value.as_type().map_err(|non_type| {
                    InterpretError::naked(format!(
                        "Cannot get struct field count of non-type value <{:?}>",
                        non_type.get_type()
                    ))
                })?;
                let r#struct = (*r#type).as_struct().map_err(|non_struct| {
                    InterpretError::naked(format!(
                        "Cannot get struct field count of non-struct type <{:?}>",
                        non_struct
                    ))
                })?;
                Value::Num(r#struct.fields.len() as i32)
            }
            Builtin::GetStructField => {
                let r#struct = values.next().expect("missing bcall arg")?;
                let field_index = values.next().expect("missing bcall arg")?;
                let r#type = r#struct.as_type().map_err(|non_type| {
                    InterpretError::naked(format!(
                        "Cannot get struct field of non-type value <{:?}>",
                        non_type.get_type()
                    ))
                })?;
                let r#struct = (*r#type).as_struct().map_err(|non_struct| {
                    InterpretError::naked(format!(
                        "Cannot get struct field of non-struct type <{:?}>",
                        non_struct
                    ))
                })?;
                let index = field_index.as_num().map_err(|non_num| {
                    InterpretError::naked(format!(
                        "Expected struct field index to be number, got: {:?}",
                        non_num.get_type()
                    ))
                })?;
                if index < 0 {
                    return Err(InterpretError::naked(format!(
                        "Struct field index expected to be positive, got: {}",
                        index
                    )));
                }
                let Some((_, field_type)) = r#struct.fields.get(index as usize) else {
                    return Err(InterpretError::naked(format!(
                        "Struct field index out of bounds (index = {}, total fields = {})",
                        index,
                        r#struct.fields.len()
                    )));
                };
                Value::Type(Box::new(field_type.clone()))
            }
            Builtin::Add => {
                let x = values.next().expect("missing bcall arg")?;
                let y = values.next().expect("missing bcall arg")?;
                let y = y.as_num().map_err(|non_num| {
                    InterpretError::naked(format!(
                        "Unsupported type for add (rhs): {:?}",
                        non_num.get_type()
                    ))
                })?;
                match x {
                    Value::Num(x) => Value::Num(x.wrapping_add(y)),
                    Value::MemoryPointer(ptr) => Value::MemoryPointer(VirtualMemoryPointer {
                        allocation: ptr.allocation,
                        offset: ptr.offset.wrapping_add(y),
                    }),
                    _ => {
                        return Err(InterpretError::naked(format!(
                            "Unsupported type for addition (lhs): {:?}",
                            x.get_type()
                        )));
                    }
                }
            }
            Builtin::Eq => {
                let x = values.next().expect("missing bcall arg")?;
                let y = values.next().expect("missing bcall arg")?;
                Value::Bool(x == y)
            }
            Builtin::Malloc => {
                let size = values.next().expect("missing bcall arg")?;
                let size = size.as_num().map_err(|non_num| {
                    InterpretError::naked(format!(
                        "Malloc expect size as i32, got: {:?}",
                        non_num.get_type()
                    ))
                })?;
                if size < 0 {
                    return Err(InterpretError::naked(format!(
                        "Negative malloc size {:?}",
                        size
                    )));
                }
                let alloc_words = vec![0; size as usize].into_boxed_slice();
                let alloc_id = self.comptime_memory.len() as AllocationId;
                self.comptime_memory.push(alloc_words);
                Value::MemoryPointer(VirtualMemoryPointer {
                    allocation: alloc_id,
                    offset: 0,
                })
            }
            Builtin::MemWrite => {
                let ptr = values.next().expect("missing bcall arg")?;
                let value = values.next().expect("missing bcall arg")?;
                let ptr = ptr.as_memptr().map_err(|v| {
                    InterpretError::naked(format!(
                        "Attempting to write to non-ptr: {:?}",
                        v.get_type()
                    ))
                })?;
                let value = value.as_num().map_err(|v| {
                    InterpretError::naked(format!(
                        "Attempting to write non-i32: {:?}",
                        v.get_type()
                    ))
                })?;
                let allocation = &mut self.comptime_memory[ptr.allocation as usize];
                if ptr.offset < 0 || ptr.offset as usize >= allocation.len() {
                    return Err(InterpretError::naked(format!(
                        "Write offset {} out of bounds",
                        ptr.offset
                    )));
                }
                allocation[ptr.offset as usize] = value;
                Value::Void
            }
            Builtin::MemRead => {
                let ptr = values.next().expect("missing bcall arg")?;
                let ptr = ptr.as_memptr().map_err(|v| {
                    InterpretError::naked(format!(
                        "Attempting to read from non-ptr: {:?}",
                        v.get_type()
                    ))
                })?;
                let allocation = &self.comptime_memory[ptr.allocation as usize];
                if ptr.offset < 0 || ptr.offset as usize >= allocation.len() {
                    return Err(InterpretError::naked(format!(
                        "Read offset {} out of bounds",
                        ptr.offset
                    )));
                }
                Value::Num(allocation[ptr.offset as usize])
            }
            builtin => {
                return Err(InterpretError::naked(format!(
                    "comptime: unimplemented builtin <{:?}>",
                    builtin
                )));
            }
        };
        Ok(v)
    }

    fn ensure_value_partially_evaluated(&mut self, value: Value) -> Result<Value, InterpretError> {
        let mut closure = match value {
            Value::Closure(closure) => closure,
            non_closure => return Ok(non_closure),
        };

        if !closure.is_comptime {
            closure.body = self.partial_eval_body(
                &closure.binds,
                closure.recursive_name.as_ref(),
                &closure.body,
                closure.captures,
            )?;
        }

        Ok(Value::Closure(closure))
    }

    fn partial_eval_body(
        &mut self,
        bind: impl AsRef<str>,
        recurse_bind: Option<impl AsRef<str>>,
        body: &Expr,
        eval_scope: ScopeId,
    ) -> Result<Expr, InterpretError> {
        let return_scope = self.current_scope;
        self.current_scope = eval_scope;
        self.bind_free(bind);
        if let Some(recurse_bind) = recurse_bind {
            self.bind_free(recurse_bind);
        }
        let result = self.partial_eval(body);
        self.current_scope = return_scope;
        result
    }

    fn partial_eval(&mut self, expr: &Expr) -> Result<Expr, InterpretError> {
        let span = expr.span;
        let kind = match &expr.kind {
            ExprKind::Var(name) => {
                let bound_value = self.get(name).ok_or_else(|| {
                    InterpretError::new(
                        format!(
                            "Undefined name {:?}\n{}",
                            name,
                            self.fmt_scope(self.current_scope)
                        ),
                        span,
                    )
                })?;

                match bound_value {
                    ValueOrFree::Value(v) => ExprKind::Value(Box::new(v.clone())),
                    ValueOrFree::Free => ExprKind::Var(name.clone()),
                }
            }
            ExprKind::Value(value) => ExprKind::Value(value.clone()),
            ExprKind::BuiltinCall(bcall) => {
                if bcall.builtin.comptime_only() {
                    self.eval_builtin(bcall)?.into()
                } else {
                    let arguments = bcall
                        .arguments
                        .iter()
                        .map(|arg| self.partial_eval(arg))
                        .collect::<Result<_, _>>()?;
                    ExprKind::BuiltinCall(Box::new(BuiltinCall {
                        builtin: bcall.builtin,
                        arguments,
                    }))
                }
            }
            ExprKind::FuncApp(func_app) => {
                let func_expr = self.partial_eval(&func_app.func_expr)?;
                let comptime_closure = match &func_expr.kind {
                    ExprKind::Value(v) => match v as &Value {
                        Value::Closure(closure) if closure.is_comptime => Some(*closure.clone()),
                        _ => None,
                    },
                    ExprKind::FuncDef(func_def) if func_def.is_comptime => {
                        Some(func_def.to_closure(self.current_scope))
                    }
                    _ => None,
                };

                if let Some(closure) = comptime_closure {
                    let apply_value = self
                        .comptime_eval(&func_app.applying_expr)
                        .push_err_span(span)?;
                    let apply_value = self
                        .ensure_value_partially_evaluated(apply_value)
                        .push_err_span(span)?;

                    let eval_result = self
                        .eval_closure(closure, apply_value)
                        .push_err_span(span)?;
                    self.ensure_value_partially_evaluated(eval_result)
                        .push_err_span(span)?
                        .into()
                } else {
                    let applying_expr = self.partial_eval(&func_app.applying_expr)?;
                    ExprKind::FuncApp(Box::new(FuncApp {
                        func_expr,
                        applying_expr,
                    }))
                }
            }
            ExprKind::FuncDef(func_def) => {
                let mut func_def = func_def.clone();

                let type_value = self.comptime_eval(&func_def.bind_type_expr)?;
                let r#type =
                    Self::as_type(type_value).push_err_span(func_def.bind_type_expr.span)?;
                func_def.bind_type_expr.kind = r#type.into();

                if !func_def.is_comptime {
                    func_def.body = self
                        .partial_eval_body(
                            &func_def.bind.name,
                            func_def.recursive_name.as_ref().map(|name| &name.name),
                            &func_def.body,
                            self.current_scope,
                        )
                        .push_err_span(span)?;
                }

                ExprKind::FuncDef(func_def)
            }
            ExprKind::IfThenElse(if_else) => {
                let condition = self.partial_eval(&if_else.condition).push_err_span(span)?;
                let true_branch = self
                    .partial_eval(&if_else.true_branch)
                    .push_err_span(span)?;
                let false_branch = self
                    .partial_eval(&if_else.false_branch)
                    .push_err_span(span)?;
                ExprKind::IfThenElse(Box::new(IfThenElse {
                    condition,
                    true_branch,
                    false_branch,
                }))
            }
            ExprKind::MemberAccess(member) => {
                if matches!(member.r#struct.kind, ExprKind::Value(_)) {
                    self.comptime_eval(expr)?.into()
                } else {
                    assert!(!matches!(&member.r#struct.kind, ExprKind::FuncDef(_)));
                    ExprKind::MemberAccess(member.clone())
                }
            }
            ExprKind::StructInit(struct_init) => {
                let r#struct = self
                    .comptime_eval(&struct_init.struct_type)
                    .push_err_span(span)?
                    .as_type()
                    .map_err(|non_type| {
                        InterpretError::new(
                            format!(
                                "Non-type value <{:?}> given as struct type",
                                non_type.get_type()
                            ),
                            span,
                        )
                    })?
                    .as_struct()
                    .map_err(|non_struct| {
                        InterpretError::new(
                            format!("Non-struct type <{:?}> given as struct type", non_struct),
                            span,
                        )
                    })?;

                let fields = struct_init
                    .fields
                    .iter()
                    .map(|init| {
                        Ok(StructInitField {
                            span: init.span,
                            name: init.name.clone(),
                            value: self.partial_eval(&init.value).push_err_span(init.span)?,
                        })
                    })
                    .collect::<Result<_, _>>()?;

                ExprKind::StructInit(Box::new(StructInit {
                    struct_type: Expr {
                        span: struct_init.struct_type.span,
                        kind: Value::Type(Box::new(Type::Struct(r#struct))).into(),
                    },
                    fields,
                }))
            }
            _ => {
                return Err(InterpretError::new(
                    format!(
                        "partial-eval: expr type <{}>, not yet implemented",
                        short_fmt(&expr.kind, 60)
                    ),
                    span,
                ));
            }
        };
        let e = Expr { kind, span };
        Ok(e)
    }
}
