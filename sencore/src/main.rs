use sencore::*;
use std::collections::HashMap;

fn main() {
    let sum_inner_param_type = StructType {
        id: 0,
        field_types: vec![Type::Word, Type::Word],
    };
    /*
     * (i, acc) ->
     * if i > zero then inner(i - 1, acc + i) else acc
     */
    let sum_inner_id = 0 as FunctionId;
    let sum_inner = AnonFunc {
        input_bind: 0, // (i, acc)
        input: Type::Struct(sum_inner_param_type.clone()),
        output: Type::Word,
        body: Expr::Let(LetExpression {
            binding: 1, // i
            assigned: Box::new(Expr::StructMemberAccess(StructMemberAccess {
                r#struct: Box::new(Expr::BindingRef(0)),
                member: 0,
            })),
            r#in: Box::new(Expr::Let(LetExpression {
                binding: 2, // acc
                assigned: Box::new(Expr::StructMemberAccess(StructMemberAccess {
                    r#struct: Box::new(Expr::BindingRef(0)),
                    member: 1,
                })),
                r#in: Box::new(Expr::IfThenElse(IfThenElse {
                    condition: Box::new(Expr::GreaterThan(
                        Box::new(Expr::BindingRef(1)),
                        Box::new(Expr::ConstWord(0)),
                    )),
                    true_branch: Box::new(Expr::FunctionInvocation(FunctionInvocation {
                        function: sum_inner_id,
                        input: Box::new(Expr::StructLiteral(StructLiteral {
                            r#type: sum_inner_param_type.clone(),
                            values: vec![
                                Expr::Add(
                                    Box::new(Expr::BindingRef(1)),
                                    Box::new(Expr::ConstWord(-1)),
                                ),
                                Expr::Add(
                                    Box::new(Expr::BindingRef(2)),
                                    Box::new(Expr::BindingRef(1)),
                                ),
                            ],
                        })),
                    })),
                    false_branch: Box::new(Expr::BindingRef(2)),
                })),
            })),
        }),
    };
    let mut functions = HashMap::new();
    functions.insert(sum_inner_id, sum_inner);
    let mut type_context = TypingContext {
        functions: &functions,
        bound: HashMap::new(),
    };
    let sum_inner = &functions[&sum_inner_id];
    type_context
        .bind(sum_inner.input_bind, sum_inner.input.clone())
        .unwrap();
    let sum_inner_res_type =
        type_check_expr(&mut type_context, &functions[&sum_inner_id].body).unwrap();
    assert_eq!(sum_inner_res_type, sum_inner.output);

    let mut eval_ctx = EvaluationContext::new(&functions);
    eval_ctx.bind(
        sum_inner.input_bind,
        Value::Struct(StructValue {
            id: sum_inner_param_type.id,
            fields: vec![Value::Word(12), Value::Word(0)],
        }),
    );
    let out = evaluate_expression(&mut eval_ctx, &sum_inner.body);
    println!("out: {:?}", out);
}
