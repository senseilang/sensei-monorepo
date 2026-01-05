use crate::Span;
use crate::ast::*;
use crate::comptime_value::*;

pub fn interpret(runtime: &Expr) -> Result<Expr, InterpretError> {
    let mut interpreter = Interpreter::new();

    interpreter.partial_eval(runtime)
}

#[derive(Debug, Clone)]
pub struct InterpretError {
    pub message: String,
    pub span: Span<usize>,
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
}

#[derive(Debug)]
struct OverlappingBindNameInScope;

impl OverlappingBindNameInScope {
    fn to_interpret_err(&self, name: impl AsRef<str>, span: Span<usize>) -> InterpretError {
        InterpretError {
            message: format!("Duplicate binding {:?} in scope", name.as_ref()),
            span,
        }
    }
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
        let parent = self.current_scope;
        self.scope_tree.push(Scope {
            parent,
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

    fn comptime_eval(&mut self, expr: &Expr) -> Result<Value, InterpretError> {
        let span = expr.span;
        let v = match &expr.kind {
            ExprKind::Value(v) => Value::clone(v),
            ExprKind::Var(name) => match self.get(&name) {
                Some(ValueOrFree::Value(v)) => v.clone(),
                Some(ValueOrFree::Free) => {
                    return Err(InterpretError {
                        message: format!("Comptime expression references free variable {:?}", name),
                        span,
                    });
                }
                None => {
                    return Err(InterpretError {
                        message: format!("Undefined reference {:?}", name),
                        span,
                    });
                }
            },
            ExprKind::IfThenElse(if_else) => {
                let cond = self.comptime_eval(&if_else.condition)?;
                let Value::Bool(cond) = cond else {
                    return Err(InterpretError {
                        message: format!("Expected boolean, got: {:?}", cond),
                        span,
                    });
                };
                if cond {
                    self.comptime_eval(&if_else.true_branch)?
                } else {
                    self.comptime_eval(&if_else.false_branch)?
                }
            }
            ExprKind::FuncDef(func_def) => self.eval_func_def(&func_def)?.into(),
            ExprKind::FuncApp(func_app) => {
                let func = self.comptime_eval(&func_app.func_expr)?;
                let apply = self.comptime_eval(&func_app.applying_expr)?;
                match func {
                    Value::Closure(closure) => self.eval_closure(*closure, apply, span)?,
                    non_func => {
                        return Err(InterpretError {
                            message: format!("Expected function, got: {:?}", non_func),
                            span,
                        });
                    }
                }
            }
            ExprKind::StructDef(struct_def) => {
                let capture = self.comptime_eval(&struct_def.capture)?;
                let fields = struct_def
                    .fields
                    .iter()
                    .map(|field| {
                        let type_value = self.comptime_eval(&field.r#type)?;
                        let r#type = Self::as_type(type_value, field.span)?;
                        Ok((field.name.name.clone(), r#type))
                    })
                    .collect::<Result<_, _>>()?;
                Value::Type(Box::new(Type::Struct(StructType {
                    def_uuid: struct_def.def_uuid,
                    fields,
                    capture,
                })))
            }
            ExprKind::BuiltinCall(call) => {
                let mut values = call.arguments.iter().map(|arg| self.comptime_eval(arg));
                match call.builtin {
                    Builtin::Eq => {
                        let x = values.next().expect("missing eq arg")?;
                        let y = values.next().expect("missing eq arg")?;
                        Value::Bool(x == y)
                    }
                    builtin => {
                        return Err(InterpretError {
                            message: format!("comptime: unimplemented builtin <{:?}>", builtin),
                            span: expr.span,
                        });
                    }
                }
            }
            ExprKind::MemberAccess(_) | ExprKind::StructInit(_) => {
                return Err(InterpretError {
                    message: format!("comptime: unimplemented: <{}>", short_fmt(&expr.kind, 60)),
                    span: expr.span,
                });
            }
        };

        Ok(v)
    }

    fn as_type(value: Value, error_span: Span<usize>) -> Result<Type, InterpretError> {
        match value {
            Value::Type(r#type) => Ok(*r#type),
            non_type => Err(InterpretError {
                message: format!(
                    "Expected closure type expression to evaluate to type, got: {:?}",
                    non_type.get_type()
                ),
                span: error_span,
            }),
        }
    }

    fn eval_func_def(&mut self, func_def: &FuncDef) -> Result<Closure, InterpretError> {
        let type_value = self.comptime_eval(&func_def.bind_type_expr)?;
        let r#type = Self::as_type(type_value, func_def.bind_type_expr.span)?;
        Ok(Closure {
            r#type,
            is_comptime: func_def.is_comptime,
            binds: func_def.func_bind.name.clone(),
            body: func_def.body.clone(),
            captures: self.current_scope,
        })
    }

    fn eval_closure(
        &mut self,
        closure: Closure,
        apply: Value,
        span: Span<usize>,
    ) -> Result<Value, InterpretError> {
        let apply_type = apply.get_type();
        if apply_type != closure.r#type {
            return Err(InterpretError {
                message: format!(
                    "Type mismatch: expected {:?}, got: {:?}",
                    apply_type, closure.r#type
                ),
                span,
            });
        }

        let return_scope = self.current_scope;
        self.current_scope = closure.captures;
        self.bind(closure.binds, apply);
        let body_res = self.comptime_eval(&closure.body);
        self.current_scope = return_scope;

        body_res
    }

    fn eval_builtin(&mut self, _bcall: &BuiltinCall) -> Result<Value, InterpretError> {
        todo!("eval_builtin")
    }

    fn partial_eval(&mut self, expr: &Expr) -> Result<Expr, InterpretError> {
        let span = expr.span;
        let kind = match &expr.kind {
            ExprKind::Var(name) => {
                let bound_value = self.get(name).ok_or_else(|| InterpretError {
                    message: format!("Undefined name {:?}", name),
                    span,
                })?;

                match bound_value {
                    ValueOrFree::Value(v) => ExprKind::Value(Box::new(v.clone())),
                    ValueOrFree::Free => ExprKind::Var(name.clone()),
                }
            }
            ExprKind::Value(v) => ExprKind::Value(v.clone()),
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
                let applying_expr = self.partial_eval(&func_app.applying_expr)?;
                let closure = match &func_expr.kind {
                    ExprKind::Value(v) => match v as &Value {
                        Value::Closure(closure) => Some(closure.clone()),
                        _ => None,
                    },
                    ExprKind::FuncDef(_) => {
                        unreachable!("func defs should be evaluated to closures?")
                    }
                    _ => None,
                };
                match closure {
                    Some(closure) if closure.is_comptime => {
                        let ExprKind::Value(apply_value) = applying_expr.kind else {
                            return Err(InterpretError {
                                message: format!("Applying non-value to comptime closure"),
                                span,
                            });
                        };
                        self.eval_closure(*closure, *apply_value, span)?.into()
                    }
                    _ => ExprKind::FuncApp(Box::new(FuncApp {
                        func_expr,
                        applying_expr,
                    })),
                }
            }
            ExprKind::FuncDef(func_def) => {
                let mut closure = self.eval_func_def(&func_def)?;
                if !closure.is_comptime {
                    let return_scope = self.current_scope;
                    self.current_scope = closure.captures;
                    self.bind_free(&func_def.func_bind.name);
                    closure.body = self.partial_eval(&closure.body)?;
                    self.current_scope = return_scope;
                }
                closure.into()
            }
            ExprKind::IfThenElse(_) => expr.kind.clone(),
            _ => {
                return Err(InterpretError {
                    message: format!(
                        "partial-eval: expr type <{}>, not yet implemented",
                        short_fmt(&expr.kind, 60)
                    ),
                    span,
                });
            }
        };
        Ok(Expr { kind, span })
    }
}
